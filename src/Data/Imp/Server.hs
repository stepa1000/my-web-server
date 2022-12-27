module Data.Imp.Server where

import Servant.Server as Servant

import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Conduit

import qualified Control.Server.News as ServerNews
import qualified Control.Server.Category as ServerCategory
import qualified Control.Server.Authorization as ServerAuthorization
import qualified Control.Logger as Logger

import qualified Control.Server.Photo as ServerPhoto
import qualified Control.Server as Server
import API.Server.Web

import qualified Data.Imp.Server.Authorization as Authorization
import qualified Data.Imp.Server.Category as Category
import qualified Data.Imp.Server.News as News
import qualified Data.Logger.Impl as ImpLogger

import Data.Maybe
import Data.Vector

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
{-
server :: Config -> IO ()
server config = do
  withHandle $ \ sh -> do
    let app = serverWithContext api 
      (getNewsPublic sh)
  where
    getNewsPublic sh mDayAt mDayUntil mDaySince mAothor mCategory mNewsNam mContent mForString mSortBy mOffSet mLimit
      = liftIO $ handleServerFind sh Nothing (Search mDayAt mDayUntil mDaySince mAothor mCategory mNewsNam mContent mForString Nothing mSortBy mOffSet mLimit)
    getNewsPrivate sh bad mDayAt mDayUntil mDaySince mAothor mCategory mNewsNam mContent mForString mSortBy mOffSet mLimit
-}
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
