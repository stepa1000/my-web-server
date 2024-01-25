{-# OPTIONS_GHC -Wwarn #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Instance (ToJSON, FromJSON, ConnectInfo) where

import Data.Yaml
import Database.Beam.Postgres as Beam

instance ToJSON ConnectInfo

instance FromJSON ConnectInfo
