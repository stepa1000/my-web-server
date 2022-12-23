{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Utils
  where

import Prelude as P

import GHC.Generics

import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Conduit

import Control.Applicative
import Control.Monad.Catch

import Data.Text
import Data.ByteString
import Data.Binary
import Data.ByteArray

import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Crypto.Hash
import Crypto.Hash.IO

import Data.Maybe
import Data.Typeable

import Data.News
import Data.User
import Data.Types

listStreamingRunSelect :: FromBackendRow Postgres a => Connection -> SqlSelect Postgres a -> IO [a] 
listStreamingRunSelect c sqls = 
  runConduitRes $ (streamingRunSelect c sqls
       ) .| sinkList


