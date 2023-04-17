{-# LANGUAGE OverloadedStrings #-}

module Data.Config
  ( getServerSettings,
    getServerSettingsTest,
  )
where

import qualified Data.Imp.Server as Server
import Data.Yaml as Y

getServerSettings :: IO Server.Config
getServerSettings = do
  decodeFileThrow "./config/server.yaml"

getServerSettingsTest :: IO Server.Config
getServerSettingsTest = do
  decodeFileThrow "./config/serverTest.yaml"
