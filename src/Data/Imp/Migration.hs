{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Imp.Migration
  ( initialSetup,
    initialSetupStep,
    migrateDB,
    migrateDBServer,
    migrationDBServerMain,
    migrationDBServerTest,
    writeInitialSetupStep,
  )
where

-- initialSetup,

-- import Database.Beam.Migrate.SQL.Tables

-- import Control.Monad.Fail (MonadFail)

import Data.Config
import Data.Imp.Database
import qualified Data.Imp.Server as Server
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Migrate as PG
import Database.Beam.Query.DataTypes

initialSetup ::
  Migration
    Postgres
    (CheckedDatabaseSettings Postgres WebServerDB)
initialSetup =
  WebServerDB
    <$> ( createTable "user" $
            UserT
              { _userName =
                  field
                    "name"
                    text
                    notNull,
                _userLogin =
                  field
                    "login"
                    text
                    notNull
                    unique,
                _userPasswordHash =
                  field
                    "passwordHash"
                    bytea
                    notNull,
                _userDateCreation =
                  field
                    "dateCreation"
                    date
                    notNull,
                _userAdmin =
                  field
                    "admin"
                    boolean
                    notNull,
                _userMakeNews =
                  field
                    "makeNews"
                    boolean
                    notNull
              }
        )
    <*> ( createTable "news" $
            NewsT
              { _newsNewsName =
                  field
                    "newsName"
                    text
                    notNull
                    unique,
                _newsLoginAuthor =
                  field
                    "loginAuthor"
                    text
                    notNull
                    unique,
                _newsNameAuthor =
                  field
                    "nameAuthor"
                    text
                    notNull,
                _newsDateCreation =
                  field
                    "dateCreation"
                    date
                    notNull,
                _newsCategory =
                  field
                    "category"
                    text
                    notNull,
                _newsContent =
                  field
                    "content"
                    text
                    notNull,
                _newsPhoto =
                  field
                    "photo"
                    bytea
                    notNull,
                _newsPublic =
                  field
                    "makeNews"
                    boolean
                    notNull
              }
        )
    <*> ( createTable "photo" $
            PhotoT
              { _photoUuid =
                  field
                    "uuid"
                    uuid
                    notNull
                    unique,
                _photoData =
                  field
                    "data"
                    text
                    notNull
              }
        )

initialSetupStep ::
  MigrationSteps
    Postgres
    ()
    (CheckedDatabaseSettings Postgres WebServerDB)
initialSetupStep =
  migrationStep
    "initial_setup"
    (const initialSetup)

writeInitialSetupStep :: IO ()
writeInitialSetupStep = PG.writeMigrationScript "sql-migration/initialMigration.sql" initialSetupStep

allowDestructive :: (MonadFail m) => BringUpToDateHooks m
allowDestructive =
  defaultUpToDateHooks
    { runIrreversibleHook = return True
    }

migrateDB ::
  Connection ->
  IO (Maybe (CheckedDatabaseSettings Postgres WebServerDB))
migrateDB conn =
  runBeamPostgresDebug putStrLn conn $
    bringUpToDateWithHooks
      allowDestructive
      PG.migrationBackend
      initialSetupStep

migrateDBServer :: Server.Config -> IO (Maybe (CheckedDatabaseSettings Postgres WebServerDB))
migrateDBServer s = do
  c <- connect (Server.confConnectionInfo s)
  cds <- migrateDB c
  close c
  return cds

migrationDBServerMain :: IO (Maybe (CheckedDatabaseSettings Postgres WebServerDB))
migrationDBServerMain = do
  s <- getServerSettings
  migrateDBServer s

migrationDBServerTest :: IO (Maybe (CheckedDatabaseSettings Postgres WebServerDB))
migrationDBServerTest = do
  s <- getServerSettingsTest
  migrateDBServer s
