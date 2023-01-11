{-# LANGUAGE OverloadedStrings #-}
module Data.Config
  ( defaultServerSettings
  , getServerSettings
  ) where

--import Servant.API
--import Servant.Server as Servant

--import Database.Beam
import Database.Beam.Postgres as Beam
--import Database.Beam.Postgres.Conduit as BPC
--import Conduit

--import qualified Control.Server.News as ServerNews
--import qualified Control.Server.Category as ServerCategory
--import qualified Control.Server.Authorization as ServerAuthorization

--import Control.Logger ((.<))
import qualified Control.Logger as Logger

--import qualified Control.Server.Photo as ServerPhoto
--import qualified Control.Server as Server
--import API.Server.Web

import qualified Data.Imp.Server.Authorization as Authorization
-- import qualified Data.Imp.Server.Category as Category
import qualified Data.Imp.Server.News as News
import qualified Data.Imp.Server as Server

import qualified Data.Logger.Impl as ImpLogger

import Data.Yaml as Y

defaultServerSettings :: IO ()
defaultServerSettings = do
  encodeFile "./config/server.yaml" $ Server.Config 
    { Server.confNews = News.Config 
        { News.confMaxLimit = 10
        }
    , Server.confFailPathToCategoryNews = "./data/categoryNews.json"
    , Server.confAuthorization = Authorization.Config
        { Authorization.confLimit = 10
        }
    , Server.confConnectionInfo = defaultConnectInfo 
    , Server.confLogger = ImpLogger.PreConfig
        { ImpLogger.preconfFilePath = "./logging/log"
        , ImpLogger.preconfMinLevel = Logger.Error
        }
    }

getServerSettings :: IO Server.Config
getServerSettings = do
  decodeFileThrow "./config/server.yaml"
