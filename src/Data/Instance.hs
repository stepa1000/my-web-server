{-# OPTIONS_GHC -Wwarn #-}

module Data.Instance (ToJSON, FromJSON) where

import Data.Yaml
import Database.Beam.Postgres as Beam

instance ToJSON ConnectInfo

instance FromJSON ConnectInfo
