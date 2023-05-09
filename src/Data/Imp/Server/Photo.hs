{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Imp.Server.Photo
  ( makeHandle,
  )
where

import qualified Control.Logger as Logger
import Control.Monad
import qualified Control.Server.Photo as SPhoto
import Data.Imp.Database
import Data.Maybe
import Data.Types
import Data.UUID
import Data.Utils
import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import System.Random
import Prelude as P

makeHandle :: Logger.Handle IO -> Connection -> SPhoto.Handle IO
makeHandle logger connectDB =
  SPhoto.Handle
    { SPhoto.hPutPhoto = hPutPhoto logger connectDB,
      SPhoto.hGetPhoto = hGetPhoto logger connectDB
    }

hGetPhoto :: Logger.Handle IO -> Connection -> Photo -> IO (Maybe Base64)
hGetPhoto logger connectDB photo = do
  Logger.logInfo logger "Get photo"
  lPhoto <-
    join . maybeToList
      <$> traverse
        ( \photo' ->
            listStreamingRunSelect connectDB $
              lookup_
                (dbPhoto webServerDB)
                ( primaryKey $
                    PhotoT
                      { _photoUuidPhoto = photo',
                        _photoData = undefined
                      }
                )
        )
        (fromText photo)
  return (_photoData <$> listToMaybe lPhoto)

hPutPhoto :: Logger.Handle IO -> Connection -> Base64 -> IO Photo
hPutPhoto logger connectDB base64 = do
  Logger.logInfo logger "Put photo"
  uuID <- randomIO @UUID
  _ <-
    BPC.runInsert connectDB $
      insert (dbPhoto webServerDB) $
        insertValues
          [ PhotoT uuID base64
          ]
  return $ toText uuID
