{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Data.Imp.Server
  ( Config (..),
    server,
    serverTest,
  )
where

import API.Server.Web
import Control.Logger ((.<))
import qualified Control.Logger as Logger
import Control.Exception
import Control.Monad
import Control.Monad.Error.Class
import qualified Control.Server as Server
import qualified Control.Server.Authorization as ServerAuthorization
import Data.ByteString as B
import qualified Data.Imp.Server.Authorization as Authorization
import qualified Data.Imp.Server.Category as Category
import qualified Data.Imp.Server.News as News
import Data.Instance
import qualified Data.Logger.Impl as ImpLogger
import Data.Maybe
import Data.News
import Data.Types
import Data.UUID
import Data.User
import Data.Vector as V
import Database.Beam
import Database.Beam.Postgres as Beam
import Network.Wai.Handler.Warp as Warp
import Servant.API
import Servant.Server as Servant
import System.Posix.Signals
import Control.Monad.Except

data Config = Config
  { confNews :: News.Config,
    confAuthorization :: Authorization.Config,
    confConnectionInfo :: ConnectInfo,
    confPortServer :: Port,
    confLogger :: ImpLogger.PreConfig
  }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

server :: IO () -> Port -> Config -> IO ()
server shutdown port config = do
  withHandle config $ \hServer -> do
    let app = serveWithContext api (serverContext hServer) (serverT hServer)
    _ <- ServerAuthorization.handleCreatInitAdmin (Server.handleAuthorization hServer) "tempAdmin" "temp" False
    Warp.runSettings (setting port shutdown) app
    return ()

serverTest :: IO () -> Port -> Config -> ((IO (), Connection) -> IO ()) -> IO ()
serverTest shutdown port config act = do
  withHandleTest config $ \(hServer, connectDB) -> do
    let app = serveWithContext api (serverContext hServer) (serverT hServer)
    nameAdmin <- ServerAuthorization.handleCreatInitAdmin (Server.handleAuthorization hServer) "tempAdmin" "temp" False
    Logger.logDebug (Server.handleLogger hServer) $ "create temp admin: " .< nameAdmin
    act (Warp.runSettings (setting port shutdown) app, connectDB)
    return ()

setting :: Port -> IO a -> Settings
setting port act =
  setInstallShutdownHandler shutdownHandler $ setPort port defaultSettings
  where
    shutdownHandler closeSocket =
      void $ installHandler sigTERM (Catch $ act >> closeSocket) Nothing

serverContext ::
  Server.Handle IO ->
  Context '[BasicAuthCheck UserPublic]
serverContext hServer = authcheck hServer :. EmptyContext

serverT ::
  Server.Handle IO ->
  GetNewsPublic Servant.Handler
    :<|> ( ( UserPublic -> GetNewsPrivate Servant.Handler
           )
             :<|> ( ( UserPublic ->
                      CategoryCreate Servant.Handler
                    )
                      :<|> ( CategoryGetTree Servant.Handler
                               :<|> ( ( UserPublic ->
                                        CategoryChange Servant.Handler
                                      )
                                        :<|> ( (UserPublic -> CreateNewsNew Servant.Handler)
                                                 :<|> ( ( UserPublic ->
                                                          CreateNewsEdit Servant.Handler
                                                        )
                                                          :<|> ( ( UserPublic ->
                                                                   UserCreate Servant.Handler
                                                                 )
                                                                   :<|> ( UserList Servant.Handler
                                                                            :<|> PhotoGet Servant.Handler
                                                                        )
                                                               )
                                                      )
                                             )
                                    )
                           )
                  )
         )
serverT sh =
  getNewsPublicS sh
    :<|> getNewsPrivateS sh
    :<|> categoryCreateS sh
    :<|> categoryGetS sh
    :<|> categoryChangeS sh
    :<|> createNewsNewS sh
    :<|> createNewsEdditS sh
    :<|> userCreateS sh
    :<|> userListS sh
    :<|> photoGetS sh

getNewsPublicS ::
  Server.Handle IO ->
  GetNewsPublic Servant.Handler
getNewsPublicS hServer mDayAt' mDayUntil' mDaySince' mAothor' mCategory' mNewsUUID' mNewsNam' mContent' mForString' mSortBy' mOffSet' mLimit' =
  liftIO $
    Server.handleServerFind
      hServer
      Nothing
      (Search mDayAt' mDayUntil' mDaySince' mAothor' mCategory' mNewsUUID' mNewsNam' mContent' mForString' Nothing mSortBy' mOffSet' mLimit')

getNewsPrivateS ::
  Server.Handle IO ->
  UserPublic ->
  GetNewsPrivate Servant.Handler
getNewsPrivateS hServer bad mDayAt' mDayUntil' mDaySince' mCategory' mNewsUUID' mNewsNam' mContent' mForString' mFlagPublished' mSortBy' mOffSet' mLimit' =
  handleErrorAuthorization hServer $
    Server.handleServerFind
      hServer
      (Just bad)
      (Search mDayAt' mDayUntil' mDaySince' (Just $ nameUser bad) mCategory' mNewsUUID' mNewsNam' mContent' mForString' mFlagPublished' mSortBy' mOffSet' mLimit')

categoryCreateS ::
  Server.Handle IO ->
  UserPublic ->
  Maybe Category ->
  Maybe Category ->
  Servant.Handler NewsCategory
categoryCreateS hServer userPub (Just categoryRoot) (Just categoryName) = do
  eNCategory <- liftIO $ try $ Server.handleCategoryCreate hServer userPub categoryRoot categoryName
  case eNCategory of
    (Right a) -> return a
    (Left err) -> throwError err
categoryCreateS hServer userPub Nothing (Just categoryName) = do
  liftIO $ Logger.logWarning (Server.handleLogger hServer) "categoryCreate: category root not Just"
  eNCategory <- liftIO $ try $ Server.handleCategoryCreate hServer userPub "General" categoryName
  case eNCategory of
    (Right a) -> return a
    (Left err) -> throwError err
categoryCreateS hServer _ _ _ = do
  liftIO $ Logger.logError (Server.handleLogger hServer) "categoryCreate: parametrs not Just"
  throwError (err401 {errBody = "categoryCreate: parametrs not Just"})

categoryGetS :: MonadIO m => Server.Handle IO -> m NewsCategory
categoryGetS hServer = liftIO $ Server.handleCategoryGet hServer

categoryChangeS ::
  Server.Handle IO ->
  UserPublic ->
  Maybe Category ->
  Maybe Category ->
  Maybe Category ->
  Servant.Handler NewsCategory
categoryChangeS hServer userPub (Just categoryName) mCategoryRoot mCategoryNewName =
  handleErrorAuthorization hServer $
    Server.handleCategoryChange hServer userPub categoryName mCategoryRoot mCategoryNewName
categoryChangeS hServer _ _ _ _ = do
  liftIO $ Logger.logError (Server.handleLogger hServer) "categoryChange: parametrs not Just"
  liftIO $ Server.handleCategoryGet hServer

createNewsNewS :: Server.Handle IO -> UserPublic -> NewsCreate -> Servant.Handler News
createNewsNewS hServer userPub newsCreate = do
  mNews <- handleErrorAuthorization hServer $ Server.handleCreateNewsNew hServer userPub newsCreate
  case mNews of
    (Just news) -> return news
    Nothing -> do
      liftIO $ Logger.logError (Server.handleLogger hServer) "userCreate: creator not admin"
      throwError $
        ServerError
          { errHTTPCode = 404,
            errReasonPhrase = "",
            errBody = fromStrict B.empty,
            errHeaders = []
          }

createNewsEdditS ::
  Server.Handle IO ->
  UserPublic ->
  Maybe UUID ->
  Maybe Content ->
  Maybe NameNews -> -- new
  Maybe Category ->
  Maybe FlagPublished ->
  [Photo] ->
  [Base64] ->
  Servant.Handler News
createNewsEdditS hServer userPub (Just uuID) content nameNews' category flagPub lPhoto lBase64 = do
  mNews <-
    handleErrorAuthorization hServer $
      Server.handleServerEditNews hServer userPub uuID content nameNews' category flagPub (V.fromList lPhoto) (V.fromList lBase64)
  case mNews of
    (Just news) -> return news
    _ -> do
      liftIO $ Logger.logError (Server.handleLogger hServer) "createNewsEddit: creator not maker news"
      throwError $
        ServerError
          { errHTTPCode = 403,
            errReasonPhrase = "creator not maker news",
            errBody = fromStrict B.empty,
            errHeaders = []
          }
createNewsEdditS hServer _ _ _ _ _ _ _ _ = do
  liftIO $ Logger.logError (Server.handleLogger hServer) "createNewsEddit: parametor are null"
  throwError $
    ServerError
      { errHTTPCode = 400,
        errReasonPhrase = "parametors not just",
        errBody = fromStrict B.empty,
        errHeaders = []
      }

userCreateS ::
  Server.Handle IO ->
  UserPublic ->
  Maybe Name ->
  Maybe Login ->
  Maybe Password ->
  Maybe FlagMakeNews ->
  Maybe FlagAdmin ->
  Servant.Handler UserPublic
userCreateS hServer userPub (Just name) (Just login) (Just password) (Just flagMakeNews) (Just flagAdmin) = do
  eUserPub <- (Servant.Handler . ExceptT . try .runExceptT . runHandler')
    (handleErrorAuthorization hServer $
      Server.handleUserCreate hServer userPub name login password flagMakeNews flagAdmin)
  case eUserPub of
    (Right (Just userPub')) -> return userPub'
    (Left serverErr) -> throwError serverErr
    _ -> do
      liftIO $ Logger.logError (Server.handleLogger hServer) "userCreate: creator not admin"
      throwError $
        ServerError
          { errHTTPCode = 404,
            errReasonPhrase = "",
            errBody = fromStrict B.empty,
            errHeaders = []
          }
userCreateS hServer _ _ _ _ _ _ = do
  liftIO $ Logger.logError (Server.handleLogger hServer) "userCreate: parametors not just"
  throwError $
    ServerError
      { errHTTPCode = 404,
        errReasonPhrase = "",
        errBody = fromStrict B.empty,
        errHeaders = []
      }

userListS ::
  Server.Handle IO ->
  Maybe OffSet ->
  Maybe Limit ->
  Servant.Handler [UserPublic]
userListS hServer mOffSet' mLimit' = liftIO $ Server.handleUserList hServer (fromMaybe 0 mOffSet') (fromMaybe 0 mLimit')

photoGetS :: Server.Handle IO -> Maybe Photo -> Servant.Handler Base64
photoGetS hServer (Just photo) = do
  mbase64 <- liftIO $ Server.handlePhotoGet hServer photo
  case mbase64 of
    (Just base64) -> return base64
    _ -> do
      liftIO $ Logger.logError (Server.handleLogger hServer) $ "photoGet: not find photo for " .< photo
      throwError $
        ServerError
          { errHTTPCode = 400,
            errReasonPhrase = "photo not found",
            errBody = fromStrict B.empty,
            errHeaders = []
          }
photoGetS hServer _ = do
  liftIO $ Logger.logError (Server.handleLogger hServer) "photoGet: parametors not just"
  throwError $
    ServerError
      { errHTTPCode = 404,
        errReasonPhrase = "",
        errBody = fromStrict B.empty,
        errHeaders = []
      }

handleErrorAuthorization :: Server.Handle IO -> IO a -> Servant.Handler a
handleErrorAuthorization hServer act = do
  outAct <- liftIO $ ServerAuthorization.handleCatchErrorAuthorization (Server.handleAuthorization hServer) act
  case outAct of
    (Left ServerAuthorization.ErrorAuthorization) ->
      throwError $
        ServerError
          { errHTTPCode = 400,
            errReasonPhrase = "authorization fail",
            errBody = fromStrict B.empty,
            errHeaders = []
          }
    (Left ServerAuthorization.ErrorAdminCheck) ->
      throwError $
        ServerError
          { errHTTPCode = 404,
            errReasonPhrase = "",
            errBody = fromStrict B.empty,
            errHeaders = []
          }
    (Left ServerAuthorization.ErrorCreatorNewsCheck) ->
      throwError $
        ServerError
          { errHTTPCode = 400,
            errReasonPhrase = "user cannot create news",
            errBody = fromStrict B.empty,
            errHeaders = []
          }
    (Right a) -> return a

authcheck :: Server.Handle IO -> BasicAuthCheck UserPublic
authcheck hServer = BasicAuthCheck $ \userPub -> do
  ServerAuthorization.hCatchErrorAuthorization
    (Server.handleAuthorization hServer)
    ( g
        <$> ServerAuthorization.handleCheckAccountStrong
          (Server.handleAuthorization hServer)
          (basicAuthDataToLogined userPub)
    )
    f
  where
    f _ = return Unauthorized
    g (Just a) = Authorized a
    g _ = Unauthorized

withHandle :: Config -> (Server.Handle IO -> IO a) -> IO a
withHandle config act = do
  connectDB <- Beam.connect $ confConnectionInfo config
  ImpLogger.withPreConf
    (confLogger config)
    ( \logger -> do
        let hNews = News.makeHandle logger (confNews config) connectDB
        let hAuthorization = Authorization.makeHandle logger (confAuthorization config) connectDB
        Category.withHandle
          logger
          connectDB
          ( \hCategory -> do
              a <-
                act $
                  Server.Handle
                    { Server.handleLogger = logger,
                      Server.handleNews = hNews,
                      Server.handleCategory = hCategory,
                      Server.handleAuthorization = hAuthorization
                    }
              close connectDB
              return a
          )
    )

withHandleTest :: Config -> ((Server.Handle IO, Connection) -> IO a) -> IO a
withHandleTest config act = do
  connectDB <- Beam.connect $ confConnectionInfo config
  ImpLogger.withPreConf
    (confLogger config)
    ( \logger -> do
        let hNews = News.makeHandle logger (confNews config) connectDB
        let hAuthorization = Authorization.makeHandle logger (confAuthorization config) connectDB
        Category.withHandle
          logger
          connectDB
          ( \hCategory -> do
              a <-
                act
                  ( Server.Handle
                      { Server.handleLogger = logger,
                        Server.handleNews = hNews,
                        Server.handleCategory = hCategory,
                        Server.handleAuthorization = hAuthorization
                      },
                    connectDB
                  )
              close connectDB
              return a
          )
    )
