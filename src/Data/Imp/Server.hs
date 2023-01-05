{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wwarn #-}
-- -Wdefault

module Data.Imp.Server 
  ( Config(..)
  , server
  ) where

import Servant.API
import Servant.Server as Servant

import Database.Beam
import Database.Beam.Postgres as Beam
--import Database.Beam.Postgres.Conduit as BPC
--import Conduit

--import qualified Control.Server.News as ServerNews
--import qualified Control.Server.Category as ServerCategory
import qualified Control.Server.Authorization as ServerAuthorization

import Control.Logger ((.<))
import qualified Control.Logger as Logger

--import qualified Control.Server.Photo as ServerPhoto
import qualified Control.Server as Server
import API.Server.Web

import qualified Data.Imp.Server.Authorization as Authorization
import qualified Data.Imp.Server.Category as Category
import qualified Data.Imp.Server.News as News
import qualified Data.Logger.Impl as ImpLogger

import Network.Wai.Handler.Warp as Warp
import System.Posix.Signals

import Control.Monad
import Control.Monad.Error.Class

-- import Data.Maybe
import Data.Vector as V
import Data.ByteString as B
import Data.Yaml

import Data.News
import Data.User
import Data.Types

data Config = Config 
  { confNews :: News.Config
  , confFailPathToCategoryNews :: FilePath
  , confAuthorization :: Authorization.Config
  , confConnectionInfo :: ConnectInfo
  , confLogger :: ImpLogger.PreConfig
  } deriving (Generic, ToJSON, FromJSON)
{-
newtype ConnectionInfo = ConnectionInfo 
  { unConnectionInfo :: ConnectInfo}
  deriving (Generic,ToJSON,FromJSON)
-}
instance ToJSON ConnectInfo
instance FromJSON ConnectInfo

server :: IO () -> Config -> IO ()
server shutdown config = do
  withHandle config $ \ sh -> do
    let app = serveWithContext api (serverContext sh) (serverT sh)
    Warp.runSettings (setting shutdown) app
    return ()
  where
    setting a = setInstallShutdownHandler shutdownHandler defaultSettings
      where
        shutdownHandler closeSocket =
          void $ installHandler sigTERM (Catch $ a >> closeSocket) Nothing
    serverContext sh = authcheck sh :. EmptyContext
    serverT sh = getNewsPublicS sh :<|> getNewsPrivateS sh :<|> categoryCreateS sh :<|> categoryGetS sh  :<|> categoryChangeS sh :<|> createNewsNewS sh :<|> 
      createNewsEdditS sh :<|> userCreateS sh :<|> userListS sh :<|> photoGetS sh
    getNewsPublicS sh mDayAt' mDayUntil' mDaySince' mAothor' mCategory' mNewsNam' mContent' mForString' mSortBy' mOffSet' mLimit'
      = liftIO $ Server.handleServerFind sh Nothing 
          (Search mDayAt' mDayUntil' mDaySince' mAothor' mCategory' mNewsNam' mContent' mForString' Nothing mSortBy' mOffSet' mLimit')
    getNewsPrivateS sh bad mDayAt' mDayUntil' mDaySince' mAothor' mCategory' mNewsNam' mContent' mForString' mFlagPublished' mSortBy' mOffSet' mLimit'
      = liftIO $ Server.handleServerFind sh (Just bad) 
        (Search mDayAt' mDayUntil' mDaySince' mAothor' mCategory' mNewsNam' mContent' mForString' mFlagPublished' mSortBy' mOffSet' mLimit' )
    categoryCreateS sh bad (Just rc) (Just nc) = liftIO $ Server.handleCategoryCreate sh bad rc nc
    categoryCreateS sh _ _ _ = do
      liftIO $ Logger.logError (Server.handleLogger sh) "categoryCreate: parametrs not Just"
      liftIO $ Server.handleCategoryGet sh
    categoryGetS sh = liftIO $ Server.handleCategoryGet sh
    categoryChangeS sh bad (Just cn) mrc mrnn 
      = liftIO $ Server.handleCategoryChange sh bad cn mrc mrnn
    categoryChangeS sh _ _ _ _ = do
      liftIO $ Logger.logError (Server.handleLogger sh) "categoryChange: parametrs not Just"
      liftIO $ Server.handleCategoryGet sh
    createNewsNewS :: Server.Handle IO -> UserPublic -> NewsCreate -> Servant.Handler News
    createNewsNewS sh bad nn = do
      mn <- liftIO $ Server.handleCreateNewsNew sh bad nn
      case mn of
        (Just n) -> return n
        Nothing -> do
          liftIO $ Logger.logError (Server.handleLogger sh) $ "userCreate: creator not admin"
          throwError $ ServerError
            { errHTTPCode = 404
            , errReasonPhrase = ""
            , errBody = fromStrict $ B.empty
            , errHeaders = []
            }
    createNewsEdditS :: Server.Handle IO 
                    -> UserPublic
                    -> Maybe NameNews -- old   
                    -> Maybe Content
                    -> Maybe NameNews -- new
                    -> Maybe Category
                    -> Maybe FlagPublished
                    -> [Photo]
                    -> [Base64]  
                    -> Servant.Handler News
    createNewsEdditS sh bad (Just nn) c nnn ca pu ph nph = do 
      mu <- liftIO $ Server.handleServerEditNews sh bad nn c nnn ca pu (V.fromList ph) (V.fromList nph)
      case mu of
        (Just u) -> return u
        _ -> do
          liftIO $ Logger.logError (Server.handleLogger sh) $ "createNewsEddit: creator not maker news"
          throwError $ ServerError
            { errHTTPCode = 403
            , errReasonPhrase = "creator not maker news"
            , errBody = fromStrict $ B.empty
            , errHeaders = []
            }
    createNewsEdditS sh _ _ _ _ _ _ _ _ = do
      liftIO $ Logger.logError (Server.handleLogger sh) $ "createNewsEddit: parametors not just"
      throwError $ ServerError
        { errHTTPCode = 400
        , errReasonPhrase = "parametors not just"
        , errBody = fromStrict $ B.empty
        , errHeaders = []
        }
    userCreateS :: Server.Handle IO 
               -> UserPublic 
               -> Maybe Name 
               -> Maybe Login 
               -> Maybe Password 
               -> Maybe FlagMakeNews 
               -> Maybe FlagAdmin 
               -> Servant.Handler UserPublic
    userCreateS sh bad (Just n) (Just l) (Just p) (Just fm) (Just fa) = do
      mu <- liftIO $ Server.handleUserCreate sh bad n l p fm fa
      case mu of
        (Just u) -> return u
        _ -> do
          liftIO $ Logger.logError (Server.handleLogger sh) $ "userCreate: creator not admin"
          throwError $ ServerError
            { errHTTPCode = 404
            , errReasonPhrase = ""
            , errBody = fromStrict $ B.empty
            , errHeaders = []
            }
    userCreateS sh _ _ _ _ _ _ = do
      liftIO $ Logger.logError (Server.handleLogger sh) $ "userCreate: parametors not just"
      throwError $ ServerError
        { errHTTPCode = 404
         , errReasonPhrase = ""
         , errBody = fromStrict $ B.empty
         , errHeaders = []
        }
    userListS sh mo ml = liftIO $ Server.handleUserList sh (maybe 0 id mo) (maybe 0 id ml)
    photoGetS :: Server.Handle IO -> Maybe Photo -> Servant.Handler Base64
    photoGetS sh (Just ph) = do
      mb <- liftIO $ Server.handlePhotoGet sh ph
      case mb of 
        (Just b) -> return b
        _ -> do
          liftIO $ Logger.logError (Server.handleLogger sh) $ "photoGet: not find photo for " .< ph
          throwError $ ServerError
            { errHTTPCode = 400
            , errReasonPhrase = "photo not found"
            , errBody = fromStrict $ B.empty
            , errHeaders = []
            }
    photoGetS sh _ = do
      liftIO $ Logger.logError (Server.handleLogger sh) $ "photoGet: parametors not just"
      throwError $ ServerError
        { errHTTPCode = 404
         , errReasonPhrase = ""
         , errBody = fromStrict $ B.empty
         , errHeaders = []
        }
 
          
authcheck :: Server.Handle IO -> BasicAuthCheck UserPublic
authcheck sh = BasicAuthCheck $ \ bad -> do
  ServerAuthorization.hCatchErrorAuthorization (Server.handleAuthorization sh)
    (fmap g $ ServerAuthorization.handleCheckAccountStrong 
      (Server.handleAuthorization sh) (basicAuthDataToLogined bad)
    ) f     
  where
    f _ = return $ Unauthorized
    g (Just a) = Authorized a
    g _ = Unauthorized

withHandle :: Config -> (Server.Handle IO -> IO a) -> IO a
withHandle conf g = do
  con <- Beam.connect $ confConnectionInfo conf
  let snh = News.makeHandle (confNews conf) con
  let ah = Authorization.makeHandle (confAuthorization conf) con
  Category.withHandle (confFailPathToCategoryNews conf) (\ ch -> do
    ImpLogger.withPreConf (confLogger conf) (\ lh -> do
      a <- g $ Server.Handle
        { Server.handleLogger = lh
        , Server.handleNews = snh
        , Server.handleCategory = ch
        , Server.handleAuthorization = ah
        }
      close con
      return a
      )
    )
