{-# LANGUAGE OverloadedStrings #-}

module Data.Config
  ( getServerSettings,
    getServerSettingsTest,
    initNewsCategoryMain,
    initNewsCategoryTest,
  )
where

import qualified Data.Imp.Server as Server
import qualified Data.Imp.Server.Category as Category
import Data.Yaml as Y

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
