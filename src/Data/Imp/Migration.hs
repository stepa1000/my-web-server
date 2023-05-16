{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Imp.Migration
  ( runMigrationFiles,
    migrationMain,
    migrationTest,
  )
where

import Data.Char
import Data.Config
import qualified Data.Imp.Server as Server
import Data.List (delete)
import Data.List.Key as LK
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.Migration
import System.Directory
import Prelude as P

-- | Check and apply all current up to the relevant version of the migration for the database.
--
-- The file migration check function is called,
-- then if the migration is not applied,
-- it is primed, otherwise it is skipped.
--
-- Migration is needed to synchronize the database with the code accessing the database.
runMigrationFiles :: ConnectInfo -> [(ScriptName, FilePath)] -> IO [MigrationResult String]
runMigrationFiles info lScriptPath = do
  connectDB <- connect info
  initMigration <- runInitMigration connectDB
  lOutMigration <-
    mapM
      ( \(name, path) -> do
          runMigrationFile connectDB name path
      )
      lScriptPath
  return $ initMigration : lOutMigration

-- | Checks the initial migration
--
-- First it checks for initialization, and if it fails,
-- it applies the initial state to the database,
-- for subsequent migrations.
runInitMigration :: Connection -> IO (MigrationResult String)
runInitMigration connectDB = do
  outMigration <- runMigration connectDB defaultOptions $ MigrationValidation MigrationInitialization
  case outMigration of
    (MigrationError _) -> runMigration connectDB defaultOptions MigrationInitialization
    a -> return a

-- | Checking the unit migration
--
-- The two functions are called, one checks if the migration
-- exists and if it does not, applies a similar function,
-- but with slightly different parameters.
--
-- To be able to create partial migrations to the database for different versions.
runMigrationFile :: Connection -> ScriptName -> FilePath -> IO (MigrationResult String)
runMigrationFile connectDB script path = do
  _ <- putStrLn $ "Check migration: " ++ script
  outMigration <- runMigration connectDB defaultOptions $ MigrationValidation $ MigrationFile script path
  case outMigration of
    (MigrationError _) -> do
      _ <- putStrLn "Migration applies"
      runMigration connectDB defaultOptions $ MigrationFile script path
    a -> do
      _ <- putStrLn "Migration skip"
      return a

-- | Combining the name of the veil and the path to it into one tuple.
unionPathName :: String -> String -> (String, String)
unionPathName path name = (name, path ++ name)

-- | Applying all available migrations to initialize from a clean database to the current version.
--
-- The body of the function lists the names of the sql-files and specifies the path to the file.
migrationAll :: ConnectInfo -> IO [MigrationResult String]
migrationAll info = do
  fails <- getDirectoryContents "./sql-migration/"
  runMigrationFiles
    info
    (unionPathName "./sql-migration/" <$> (LK.sort (read @Int . takeWhile isDigit) . delete "." . delete "..") fails)

migrationMain :: IO [MigrationResult String]
migrationMain = do
  config <- getServerSettings
  migrationAll (Server.confConnectionInfo config)

migrationTest :: IO [MigrationResult String]
migrationTest = do
  config <- getServerSettingsTest
  migrationAll (Server.confConnectionInfo config)
