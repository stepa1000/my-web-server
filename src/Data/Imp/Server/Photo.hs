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
makeHandle hl c =
  SPhoto.Handle
    { SPhoto.hPutPhoto = hPutPhoto hl c,
      SPhoto.hGetPhoto = hGetPhoto hl c
    }

hGetPhoto :: Logger.Handle IO -> Connection -> Photo -> IO (Maybe Base64)
hGetPhoto hl c p' = do
  Logger.logInfo hl "Get photo"
  l <-
    join . maybeToList
      <$> traverse
        ( \p ->
            listStreamingRunSelect c $
              lookup_
                (dbPhoto webServerDB)
                ( primaryKey $
                    PhotoT
                      { _photoUuid = p,
                        _photoData = undefined
                      }
                )
        )
        (fromText p')
  return (_photoData <$> listToMaybe l)

hPutPhoto :: Logger.Handle IO -> Connection -> Base64 -> IO Photo
hPutPhoto hl c b = do
  Logger.logInfo hl "Put photo"
  u <- randomIO @UUID
  _ <-
    BPC.runInsert c $
      insert (dbPhoto webServerDB) $
        insertValues
          [ PhotoT u b
          ]
  return $ toText u
