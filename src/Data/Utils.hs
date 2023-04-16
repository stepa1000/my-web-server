{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Utils
  ( listStreamingRunSelect,
    concatV_,
    arrayRemove,
  )
where

import Conduit
import Data.Vector
import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Prelude as P

listStreamingRunSelect :: FromBackendRow Postgres a => Connection -> SqlSelect Postgres a -> IO [a]
listStreamingRunSelect c sqls =
  runConduitRes $ streamingRunSelect c sqls .| sinkList

-- | join two arrays in sql expression
concatV_ ::
  QGenExpr QValueContext Postgres s (Vector a) ->
  QGenExpr QValueContext Postgres s (Vector a) ->
  QGenExpr QValueContext Postgres s (Vector a)
concatV_ = customExpr_ (\v1 v2 -> v1 <> " || " <> v2)

-- | deleting an item from an array
arrayRemove ::
  QGenExpr QValueContext Postgres s (Vector a) ->
  QGenExpr QValueContext Postgres s a ->
  QGenExpr QValueContext Postgres s (Vector a)
arrayRemove = customExpr_ (\v1 a -> "array_remove ( " <> v1 <> ", " <> a <> " )")
