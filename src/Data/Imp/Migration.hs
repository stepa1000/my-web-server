{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Imp.Migration
  ( runMigrationFiles,
    migrationMain,
    migrationTest,
  )
where

import Data.Config
import qualified Data.Imp.Server as Server
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.Migration
import Prelude as P

-- | Check and apply all current up to the relevant version of the migration for the database.
--
-- The file migration check function is called,
-- then if the migration is not applied,
-- it is primed, otherwise it is skipped.
--
-- Migration is needed to synchronize the database with the code accessing the database.
runMigrationFiles :: ConnectInfo -> [(ScriptName, FilePath)] -> IO [MigrationResult String]
runMigrationFiles ci lcnfp = do
  c <- connect ci
  im <- runInitMigration c
  lm <-
    mapM
      ( \(sn, fp) -> do
          runMigrationFile c sn fp
      )
      lcnfp
  return $ im : lm

-- | Checks the initial migration
--
-- First it checks for initialization, and if it fails,
-- it applies the initial state to the database,
-- for subsequent migrations.
runInitMigration :: Connection -> IO (MigrationResult String)
runInitMigration c = do
  me <- runMigration c defaultOptions $ MigrationValidation MigrationInitialization
  case me of
    (MigrationError _) -> runMigration c defaultOptions MigrationInitialization
    a -> return a

-- | Checking the unit migration
--
-- The two functions are called, one checks if the migration
-- exists and if it does not, applies a similar function,
-- but with slightly different parameters.
--
-- To be able to create partial migrations to the database for different versions.
runMigrationFile :: Connection -> ScriptName -> FilePath -> IO (MigrationResult String)
runMigrationFile c sn fp = do
  _ <- putStrLn $ "Check migration: " ++ sn
  me <- runMigration c defaultOptions $ MigrationValidation $ MigrationFile sn fp
  case me of
    (MigrationError _) -> do
      _ <- putStrLn "Migration applies"
      runMigration c defaultOptions $ MigrationFile sn fp
    a -> do
      _ <- putStrLn "Migration skip"
      return a

-- | Applying all available migrations to initialize from a clean database to the current version.
--
-- The body of the function lists the names of the sql-files and specifies the path to the file.
migrationAll :: ConnectInfo -> IO [MigrationResult String]
migrationAll ci =
  runMigrationFiles
    ci
    [ f "a_create_user.sql",
      f "b_create_news.sql",
      f "b_create_photo.sql",
      f "b_addCategoryNews.sql"
    ]
  where
    f t = (t, "./sql-migration/" ++ t)

migrationMain :: IO [MigrationResult String]
migrationMain = do
  c <- getServerSettings
  migrationAll (Server.confConnectionInfo c)

migrationTest :: IO [MigrationResult String]
migrationTest = do
  c <- getServerSettingsTest
  migrationAll (Server.confConnectionInfo c)
