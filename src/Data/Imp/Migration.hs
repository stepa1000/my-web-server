{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Imp.Migration
  ( runMigrationFile,
    migrationMain,
    migrationTest,
  )
where

import Data.Config
import qualified Data.Imp.Server as Server
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.Migration
import Prelude as P

runMigrationFile :: ConnectInfo -> IO (MigrationResult String)
runMigrationFile ci = do
  c <- connect ci
  runMigrations c defaultOptions [MigrationInitialization, MigrationDirectory "sql-migration"]

migrationMain :: IO (MigrationResult String)
migrationMain = do
  c <- getServerSettings
  runMigrationFile (Server.confConnectionInfo c)

migrationTest :: IO (MigrationResult String)
migrationTest = do
  c <- getServerSettingsTest
  runMigrationFile (Server.confConnectionInfo c)
