{-# LANGUAGE OverloadedStrings #-}
module Data.Config
  (
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
import qualified Data.Imp.Server as Server

import qualified Data.Logger.Impl as ImpLogger

import Network.Wai.Handler.Warp as Warp
import System.Posix.Signals

import Control.Monad
import Control.Monad.Error.Class

-- import Data.Maybe
import Data.Vector as V
import Data.ByteString as B
import Data.Yaml as Y

import Data.News
import Data.User
import Data.Types

defaultServerSettings = do
  encodeFile $ Server.Config 
    { confNews = News.Config 
        { News.confMaxLimit = 10
        }
    , confFilePathToCategryNews = "./data/categoryNews.json"
    , confAuthorization = Authorization.Config
        { Authorization.confLimit = 10
        }
    }
