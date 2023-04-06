{-# LANGUAGE OverloadedStrings #-}

module Data.Config
  ( defaultServerSettings,
    getServerSettings,
    testDefaultServerSettings,
    getServerSettingsTest,
    initNewsCategoryMain,
    initNewsCategoryTest,
  )
where

import qualified Control.Logger as Logger
import qualified Data.Imp.Server as Server
import qualified Data.Imp.Server.Authorization as Authorization
import qualified Data.Imp.Server.Category as Category
import qualified Data.Imp.Server.News as News
import qualified Data.Logger.Impl as ImpLogger
import Data.Yaml as Y
import Database.Beam.Postgres as Beam

defaultServerSettings :: IO ()
defaultServerSettings = do
  encodeFile "./config/server.yaml" $
    Server.Config
      { Server.confNews =
          News.Config
            { News.confMaxLimit = 10
            },
        Server.confFailPathToCategoryNews = "./data/categoryNews.json",
        Server.confAuthorization =
          Authorization.Config
            { Authorization.confLimit = 10
            },
        Server.confConnectionInfo = mainDBConnect,
        Server.confLogger =
          ImpLogger.PreConfig
            { ImpLogger.preconfFilePath = "./logging/log",
              ImpLogger.preconfMinLevel = Logger.Error
            }
      }

testDefaultServerSettings :: IO ()
testDefaultServerSettings = do
  encodeFile "./config/serverTest.yaml" $
    Server.Config
      { Server.confNews =
          News.Config
            { News.confMaxLimit = 10
            },
        Server.confFailPathToCategoryNews = "./data/categoryNewsTest.json",
        Server.confAuthorization =
          Authorization.Config
            { Authorization.confLimit = 10
            },
        Server.confConnectionInfo = testDBConnect,
        Server.confLogger =
          ImpLogger.PreConfig
            { ImpLogger.preconfFilePath = "./logging/logTest",
              ImpLogger.preconfMinLevel = Logger.Debug
            }
      }

mainDBConnect :: ConnectInfo
mainDBConnect =
  defaultConnectInfo {connectUser = "stepan", connectDatabase = "newsdb", connectPassword = "123"}

testDBConnect :: ConnectInfo
testDBConnect =
  defaultConnectInfo {connectUser = "stepan", connectDatabase = "testdb", connectPassword = "123"}

initNewsCategoryMain :: IO ()
initNewsCategoryMain = do
  Category.initNewsCategory' "./data/categoryNews.json"

initNewsCategoryTest :: IO ()
initNewsCategoryTest = do
  Category.initNewsCategory' "./data/categoryNewsTest.json"

getServerSettings :: IO Server.Config
getServerSettings = do
  decodeFileThrow "./config/server.yaml"

getServerSettingsTest :: IO Server.Config
getServerSettingsTest = do
  decodeFileThrow "./config/serverTest.yaml"
