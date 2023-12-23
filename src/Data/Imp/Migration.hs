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
  )
where

import Control.Arrow
import Data.Config
import Data.Imp.Database
import qualified Data.Imp.OldDataBase.Database as Old
import qualified Data.Imp.OldDataBase.DatabaseMk1 as OldMk1
import qualified Data.Imp.Server as Server
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Migrate as PG
import Database.Beam.Query.DataTypes
import Unsafe.Coerce

initialSetup ::
  Migration
    Postgres
    (CheckedDatabaseSettings Postgres Old.WebServerDB)
initialSetup =
  Old.WebServerDB
    <$> ( createTable "user" $
            Old.UserT
              { Old._userName =
                  field
                    "name"
                    text
                    notNull,
                Old._userLogin =
                  field
                    "login"
                    text
                    notNull
                    unique,
                Old._userPasswordHash =
                  field
                    "password_hash"
                    bytea
                    notNull,
                Old._userDateCreation =
                  field
                    "date_creation"
                    date
                    notNull,
                Old._userAdmin =
                  field
                    "admin"
                    boolean
                    notNull,
                Old._userMakeNews =
                  field
                    "make_news"
                    boolean
                    notNull
              }
        )
    <*> ( createTable "news" $
            Old.NewsT
              { Old._newsNewsName =
                  field
                    "news_name"
                    text
                    notNull,
                Old._newsLoginAuthor =
                  field
                    "login_author"
                    text
                    notNull,
                Old._newsNameAuthor =
                  field
                    "name_author"
                    text
                    notNull,
                Old._newsDateCreation =
                  field
                    "date_creation"
                    date
                    notNull,
                Old._newsCategory =
                  field
                    "category"
                    text
                    notNull,
                Old._newsContent =
                  field
                    "content"
                    text
                    notNull,
                Old._newsPhoto =
                  field
                    "photo"
                    bytea
                    notNull,
                Old._newsPublic =
                  field
                    "public"
                    boolean
                    notNull
              }
        )
    <*> ( createTable "photo" $
            Old.PhotoT
              { Old._photoUuid =
                  field
                    "uuid"
                    uuid
                    notNull
                    unique,
                Old._photoData =
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
    (CheckedDatabaseSettings Postgres Old.WebServerDB)
initialSetupStep =
  migrationStep
    "initial_setup"
    (const initialSetup)

addUUIDsfromNewsCategory ::
  MigrationSteps
    Postgres
    (CheckedDatabaseSettings Postgres Old.WebServerDB)
    (CheckedDatabaseSettings Postgres OldMk1.WebServerDB)
addUUIDsfromNewsCategory =
  migrationStep
    "add uuid from News and add Category"
    ( \(Old.WebServerDB userTabl newsTabl photoTabl) -> do
        newsTabl' <- alterTable newsTabl $ \a -> do
          newsUUIDNews <- addColumn $ field "uuid_news" uuid notNull unique
          return $
            OldMk1.NewsT
              newsUUIDNews
              (Old._newsNewsName a)
              (Old._newsLoginAuthor a)
              (Old._newsNameAuthor a)
              (Old._newsDateCreation a)
              (Old._newsCategory a)
              (Old._newsContent a)
              (Old._newsPhoto a)
              (Old._newsPublic a)
        photoTabl' <- alterTable photoTabl $ \a -> do
          uuidPhoto <- renameColumnTo "uuid_photo" (Old._photoUuid a)
          return $ OldMk1.PhotoT uuidPhoto (Old._photoData a)
        catTabl <-
          createTable "category" $
            OldMk1.CategoryT
              { OldMk1._categoryUuidCategory =
                  field
                    "uuid_category"
                    uuid
                    notNull
                    unique,
                OldMk1._categoryCategoryName =
                  field
                    "category_name"
                    text
                    notNull,
                OldMk1._categoryParent =
                  field
                    "parent"
                    text,
                OldMk1._categoryChild =
                  field
                    "child"
                    (unboundedArray text)
              }
        return $
          OldMk1.WebServerDB
            (unsafeCoerce userTabl)
            newsTabl'
            photoTabl'
            catTabl
    )

addUniqueForCategoryName ::
  MigrationSteps
    Postgres
    (CheckedDatabaseSettings Postgres OldMk1.WebServerDB)
    (CheckedDatabaseSettings Postgres OldMk1.WebServerDB)
addUniqueForCategoryName =
  migrationStep
    "add unique for category name"
    ( \(OldMk1.WebServerDB userTabl newsTabl photoTabl categoryTabl) -> do
        categoryTabl' <- alterTable categoryTabl $ \a -> do
          dropColumn $ OldMk1._categoryCategoryName a
          categoryNameColumn <-
            addColumn $
              field
                "category_name"
                text
                unique
          return $
            OldMk1.CategoryT
              { OldMk1._categoryUuidCategory = OldMk1._categoryUuidCategory a,
                OldMk1._categoryCategoryName = categoryNameColumn,
                OldMk1._categoryParent = OldMk1._categoryParent a,
                OldMk1._categoryChild = OldMk1._categoryChild a
              }
        return $ OldMk1.WebServerDB userTabl newsTabl photoTabl categoryTabl'
    )

deleteChaildsForCategory ::
  MigrationSteps
    Postgres
    (CheckedDatabaseSettings Postgres OldMk1.WebServerDB)
    (CheckedDatabaseSettings Postgres WebServerDB)
deleteChaildsForCategory =
  migrationStep
    "delete chailds for category"
    ( \(OldMk1.WebServerDB userTabl newsTabl photoTabl categoryTabl) -> do
        categoryTabl' <- alterTable categoryTabl $ \a -> do
          dropColumn $ OldMk1._categoryChild a
          return $
            CategoryT
              { _categoryUuidCategory = OldMk1._categoryUuidCategory a,
                _categoryCategoryName = OldMk1._categoryCategoryName a,
                _categoryParent = OldMk1._categoryParent a
              }
        return $ WebServerDB (unsafeCoerce userTabl) (unsafeCoerce newsTabl) (unsafeCoerce photoTabl) categoryTabl'
    )

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
      (deleteChaildsForCategory <<< addUniqueForCategoryName <<< addUUIDsfromNewsCategory <<< initialSetupStep)

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
