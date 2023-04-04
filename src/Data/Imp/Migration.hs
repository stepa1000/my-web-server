{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Imp.Migration
  (
  )
where

-- initialSetup,

import Data.Imp.Database
import Database.Beam.Migrate
-- import Database.Beam.Migrate.SQL.Tables
import Database.Beam.Postgres
-- import Database.Beam.Postgres.Migrate
import Database.Beam.Query.DataTypes

{-
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
                    notNull
                    unique,
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
                    notNull
                    unique,
                _userDateCreation =
                  field
                    "dateCreation"
                    date
                    notNull
                    unique,
                _userAdmin =
                  field
                    "admin"
                    boolean
                    notNull
                    unique,
                _userMakeNews =
                  field
                    "makeNews"
                    boolean
                    notNull
                    unique
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

              }
        )
-}
