{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Utils
  ( listStreamingRunSelect
  ) where

import Prelude as P

import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Conduit

listStreamingRunSelect :: FromBackendRow Postgres a => Connection -> SqlSelect Postgres a -> IO [a] 
listStreamingRunSelect c sqls = 
  runConduitRes $ streamingRunSelect c sqls .| sinkList


