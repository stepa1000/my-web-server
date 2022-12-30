{-# LANGUAGE OverloadedStrings #-}
module Data.Imp.Server where

import Servant.API
import Servant.Server as Servant

import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Conduit

import qualified Control.Server.News as ServerNews
import qualified Control.Server.Category as ServerCategory
import qualified Control.Server.Authorization as ServerAuthorization

import Control.Logger ((.<))
import qualified Control.Logger as Logger

import qualified Control.Server.Photo as ServerPhoto
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

import Data.Maybe
import Data.Vector as V
import Data.ByteString as B

import Data.News
import Data.User
import Data.Types

data Config = Config 
  { confNews :: News.Config
  , confFailPathToCategoryNews :: FilePath
  , confAuthorization :: Authorization.Config
  , confConnectionInfo :: ConnectInfo
  , confLogger :: ImpLogger.Config
  }

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
    serverT sh = getNewsPublic sh :<|> getNewsPrivate sh :<|> categoryCreate sh :<|> categoryGet sh  :<|> categoryChange sh :<|> createNewsNew sh :<|> 
      createNewsEddit sh :<|> userCreate sh :<|> userList sh :<|> photoGet sh
    getNewsPublic sh mDayAt mDayUntil mDaySince mAothor mCategory mNewsNam mContent mForString mSortBy mOffSet mLimit
      = liftIO $ Server.handleServerFind sh Nothing (Search mDayAt mDayUntil mDaySince mAothor mCategory mNewsNam mContent mForString Nothing mSortBy mOffSet mLimit)
    getNewsPrivate sh bad mDayAt mDayUntil mDaySince mAothor mCategory mNewsNam mContent mForString mFlagPublished mSortBy mOffSet mLimit
      = liftIO $ Server.handleServerFind sh (Just bad) 
        (Search mDayAt mDayUntil mDaySince mAothor mCategory mNewsNam mContent mForString mFlagPublished mSortBy mOffSet mLimit )
    categoryCreate sh bad (Just rc) (Just nc) = Server.handleCategoryCreate sh bad rc nc
    categoryCreate sh bad mrc mnc = do
      Logger.logError (Server.handleLogger sh) "categoryCreate: parametrs not Just"
      liftIO $ Server.handleCategoryGet sh
    categoryGet sh = liftIO $ Server.handleCategoryGet sh
    categoryChange sh bad (Just cn) mrc mrnn 
      = liftIO $ Server.handleCategoryChange sh bad cn mrc mrnn
    categoryChange sh bad mcn _ _ = do
      liftIO $ Logger.logError (Server.handleLogger sh) "categoryChange: parametrs not Just"
      liftIO $ Server.handleCategoryGet sh
    createNewsNew sh bad nn = do
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
    createNewsEddit sh bad nn c nnn ca pu ph nph 
      = liftIO $ Server.handleServerEditNews sh bad nn c nnn ca pu (V.fromList ph) (V.fromList nph)
    userCreate :: Server.Handle IO 
               -> UserPublic 
               -> Maybe Name 
               -> Maybe Login 
               -> Maybe Password 
               -> Maybe FlagMakeNews 
               -> Maybe FlagAdmin 
               -> Servant.Handler UserPublic
    userCreate sh bad (Just n) (Just l) (Just p) (Just fm) (Just fa) = do
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
    userCreate sh bad _ _ _ _ _ = do
      liftIO $ Logger.logError (Server.handleLogger sh) $ "userCreate: parametors not just"
      throwError $ ServerError
        { errHTTPCode = 404
         , errReasonPhrase = ""
         , errBody = fromStrict $ B.empty
         , errHeaders = []
        }
    userList sh mo ml = liftIO $ Server.handleUserList sh (maybe 0 id mo) (maybe 0 id ml)
    photoGet :: Server.Handle IO -> Maybe Photo -> Servant.Handler Base64
    photoGet sh (Just ph) = do
      mb <- liftIO $ Server.handlePhotoGet sh ph
      case mb of 
        (Just b) -> return b
        _ -> do
          liftIO $ Logger.logError (Server.handleLogger sh) $ "categoryChange: not find photo for " .< ph
          throwError $ ServerError
            { errHTTPCode = 400
            , errReasonPhrase = "photo not found"
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
    ImpLogger.withHandle (confLogger conf) (\ lh -> do
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
