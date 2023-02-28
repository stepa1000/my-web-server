{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wwarn #-}

module Data.Imp.Server
  ( Config (..),
    server,
    serverTest,
  )
where

import API.Server.Web
import Control.Logger ((.<))
import qualified Control.Logger as Logger
import Control.Monad
import Control.Monad.Error.Class
import qualified Control.Server as Server
import qualified Control.Server.Authorization as ServerAuthorization
import Data.ByteString as B
import qualified Data.Imp.Server.Authorization as Authorization
import qualified Data.Imp.Server.Category as Category
import qualified Data.Imp.Server.News as News
import qualified Data.Logger.Impl as ImpLogger
import Data.Maybe
import Data.News
import Data.Types
import Data.User
import Data.Vector as V
import Data.Yaml
import Database.Beam
import Database.Beam.Postgres as Beam
import Network.Wai.Handler.Warp as Warp
import Servant.API
import Servant.Server as Servant
import System.Posix.Signals

data Config = Config
  { confNews :: News.Config,
    confFailPathToCategoryNews :: FilePath,
    confAuthorization :: Authorization.Config,
    confConnectionInfo :: ConnectInfo,
    confLogger :: ImpLogger.PreConfig
  }
  deriving (Generic, ToJSON, FromJSON)

instance ToJSON ConnectInfo

instance FromJSON ConnectInfo

server :: IO () -> Config -> IO ()
server shutdown config = do
  withHandle config $ \sh -> do
    let app = serveWithContext api (serverContext sh) (serverT sh)
    _ <- ServerAuthorization.handleCreatInitAdmin (Server.handleAuthorization sh) "tempAdmin" "temp" False
    Warp.runSettings (setting shutdown) app
    return ()

serverTest :: IO () -> Config -> ((IO (), Connection) -> IO ()) -> IO ()
serverTest shutdown config g = do
  withHandleTest config $ \(sh, c) -> do
    let app = serveWithContext api (serverContext sh) (serverT sh)
    mu <- ServerAuthorization.handleCreatInitAdmin (Server.handleAuthorization sh) "tempAdmin" "temp" False
    Logger.logDebug (Server.handleLogger sh) $ "create temp admin: " .< mu
    g (Warp.runSettings (setting shutdown) app, c)
    return ()

setting :: IO a -> Settings
setting a =
  setInstallShutdownHandler shutdownHandler defaultSettings
  where
    shutdownHandler closeSocket =
      void $ installHandler sigTERM (Catch $ a >> closeSocket) Nothing

serverContext ::
  Server.Handle IO ->
  Context '[BasicAuthCheck UserPublic]
serverContext sh = authcheck sh :. EmptyContext

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
  MonadIO m =>
  Server.Handle IO ->
  Maybe DayAt ->
  Maybe DayUntil ->
  Maybe DaySince ->
  Maybe Name ->
  Maybe Category ->
  Maybe NewsName ->
  Maybe Content ->
  Maybe ForString ->
  Maybe SortBy ->
  Maybe OffSet ->
  Maybe Limit ->
  m [News]
getNewsPublicS sh mDayAt' mDayUntil' mDaySince' mAothor' mCategory' mNewsNam' mContent' mForString' mSortBy' mOffSet' mLimit' =
  liftIO $
    Server.handleServerFind
      sh
      Nothing
      (Search mDayAt' mDayUntil' mDaySince' mAothor' mCategory' mNewsNam' mContent' mForString' Nothing mSortBy' mOffSet' mLimit')

getNewsPrivateS ::
  Server.Handle IO ->
  UserPublic ->
  Maybe DayAt ->
  Maybe DayUntil ->
  Maybe DaySince ->
  Maybe Category ->
  Maybe NewsName ->
  Maybe Content ->
  Maybe ForString ->
  Maybe FlagPublished ->
  Maybe SortBy ->
  Maybe OffSet ->
  Maybe Limit ->
  Servant.Handler [News]
getNewsPrivateS sh bad mDayAt' mDayUntil' mDaySince' mCategory' mNewsNam' mContent' mForString' mFlagPublished' mSortBy' mOffSet' mLimit' =
  handleErrorAuthorization sh $
    Server.handleServerFind
      sh
      (Just bad)
      (Search mDayAt' mDayUntil' mDaySince' (Just $ nameUser bad) mCategory' mNewsNam' mContent' mForString' mFlagPublished' mSortBy' mOffSet' mLimit')

categoryCreateS ::
  MonadIO m =>
  Server.Handle IO ->
  UserPublic ->
  Maybe Category ->
  Maybe Category ->
  m NewsCategory
categoryCreateS sh bad (Just rc) (Just nc) = liftIO $ Server.handleCategoryCreate sh bad rc nc
categoryCreateS sh _ _ _ = do
  liftIO $ Logger.logError (Server.handleLogger sh) "categoryCreate: parametrs not Just"
  liftIO $ Server.handleCategoryGet sh

categoryGetS :: MonadIO m => Server.Handle IO -> m NewsCategory
categoryGetS sh = liftIO $ Server.handleCategoryGet sh

categoryChangeS ::
  Server.Handle IO ->
  UserPublic ->
  Maybe Category ->
  Maybe Category ->
  Maybe Category ->
  Servant.Handler NewsCategory
categoryChangeS sh bad (Just cn) mrc mrnn =
  handleErrorAuthorization sh $ Server.handleCategoryChange sh bad cn mrc mrnn
categoryChangeS sh _ _ _ _ = do
  liftIO $ Logger.logError (Server.handleLogger sh) "categoryChange: parametrs not Just"
  liftIO $ Server.handleCategoryGet sh

createNewsNewS :: Server.Handle IO -> UserPublic -> NewsCreate -> Servant.Handler News
createNewsNewS sh bad nn = do
  mn <- handleErrorAuthorization sh $ Server.handleCreateNewsNew sh bad nn
  case mn of
    (Just n) -> return n
    Nothing -> do
      liftIO $ Logger.logError (Server.handleLogger sh) "userCreate: creator not admin"
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
  Maybe NameNews -> -- old
  Maybe Content ->
  Maybe NameNews -> -- new
  Maybe Category ->
  Maybe FlagPublished ->
  [Photo] ->
  [Base64] ->
  Servant.Handler News
createNewsEdditS sh bad (Just nn) c nnn ca pu ph nph = do
  mu <- handleErrorAuthorization sh $ Server.handleServerEditNews sh bad nn c nnn ca pu (V.fromList ph) (V.fromList nph)
  case mu of
    (Just u) -> return u
    _ -> do
      liftIO $ Logger.logError (Server.handleLogger sh) "createNewsEddit: creator not maker news"
      throwError $
        ServerError
          { errHTTPCode = 403,
            errReasonPhrase = "creator not maker news",
            errBody = fromStrict B.empty,
            errHeaders = []
          }
createNewsEdditS sh _ _ _ _ _ _ _ _ = do
  liftIO $ Logger.logError (Server.handleLogger sh) "createNewsEddit: parametors not just"
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
userCreateS sh bad (Just n) (Just l) (Just p) (Just fm) (Just fa) = do
  mu <- handleErrorAuthorization sh $ Server.handleUserCreate sh bad n l p fm fa
  case mu of
    (Just u) -> return u
    _ -> do
      liftIO $ Logger.logError (Server.handleLogger sh) "userCreate: creator not admin"
      throwError $
        ServerError
          { errHTTPCode = 404,
            errReasonPhrase = "",
            errBody = fromStrict B.empty,
            errHeaders = []
          }
userCreateS sh _ _ _ _ _ _ = do
  liftIO $ Logger.logError (Server.handleLogger sh) "userCreate: parametors not just"
  throwError $
    ServerError
      { errHTTPCode = 404,
        errReasonPhrase = "",
        errBody = fromStrict B.empty,
        errHeaders = []
      }

userListS ::
  MonadIO m =>
  Server.Handle IO ->
  Maybe OffSet ->
  Maybe Limit ->
  m [UserPublic]
userListS sh mo ml = liftIO $ Server.handleUserList sh (fromMaybe 0 mo) (fromMaybe 0 ml)

photoGetS :: Server.Handle IO -> Maybe Photo -> Servant.Handler Base64
photoGetS sh (Just ph) = do
  mb <- liftIO $ Server.handlePhotoGet sh ph
  case mb of
    (Just b) -> return b
    _ -> do
      liftIO $ Logger.logError (Server.handleLogger sh) $ "photoGet: not find photo for " .< ph
      throwError $
        ServerError
          { errHTTPCode = 400,
            errReasonPhrase = "photo not found",
            errBody = fromStrict B.empty,
            errHeaders = []
          }
photoGetS sh _ = do
  liftIO $ Logger.logError (Server.handleLogger sh) "photoGet: parametors not just"
  throwError $
    ServerError
      { errHTTPCode = 404,
        errReasonPhrase = "",
        errBody = fromStrict B.empty,
        errHeaders = []
      }

handleErrorAuthorization :: Server.Handle IO -> IO a -> Servant.Handler a
handleErrorAuthorization h m = do
  e <- liftIO $ ServerAuthorization.handleCatchErrorAuthorization (Server.handleAuthorization h) m
  case e of
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
authcheck sh = BasicAuthCheck $ \bad -> do
  ServerAuthorization.hCatchErrorAuthorization
    (Server.handleAuthorization sh)
    ( g
        <$> ServerAuthorization.handleCheckAccountStrong
          (Server.handleAuthorization sh)
          (basicAuthDataToLogined bad)
    )
    f
  where
    f _ = return Unauthorized
    g (Just a) = Authorized a
    g _ = Unauthorized

withHandle :: Config -> (Server.Handle IO -> IO a) -> IO a
withHandle conf g = do
  con <- Beam.connect $ confConnectionInfo conf
  ImpLogger.withPreConf
    (confLogger conf)
    ( \lh -> do
        let snh = News.makeHandle lh (confNews conf) con
        let ah = Authorization.makeHandle lh (confAuthorization conf) con
        Category.withHandle
          lh
          (confFailPathToCategoryNews conf)
          ( \ch -> do
              a <-
                g $
                  Server.Handle
                    { Server.handleLogger = lh,
                      Server.handleNews = snh,
                      Server.handleCategory = ch,
                      Server.handleAuthorization = ah
                    }
              close con
              return a
          )
    )

withHandleTest :: Config -> ((Server.Handle IO, Connection) -> IO a) -> IO a
withHandleTest conf g = do
  con <- Beam.connect $ confConnectionInfo conf
  ImpLogger.withPreConf
    (confLogger conf)
    ( \lh -> do
        let snh = News.makeHandle lh (confNews conf) con
        let ah = Authorization.makeHandle lh (confAuthorization conf) con
        Category.withHandle
          lh
          (confFailPathToCategoryNews conf)
          ( \ch -> do
              a <-
                g
                  ( Server.Handle
                      { Server.handleLogger = lh,
                        Server.handleNews = snh,
                        Server.handleCategory = ch,
                        Server.handleAuthorization = ah
                      },
                    con
                  )
              close con
              return a
          )
    )
