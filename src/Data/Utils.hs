-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Utils
  ( listStreamingRunSelect,
  )
where

import Conduit
import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Prelude as P

listStreamingRunSelect :: FromBackendRow Postgres a => Connection -> SqlSelect Postgres a -> IO [a]
listStreamingRunSelect c sqls =
  runConduitRes $ streamingRunSelect c sqls .| sinkList
