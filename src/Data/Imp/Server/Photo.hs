{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Imp.Server.Photo 
  ( makeHandle
  ) where

import Prelude as P

import GHC.Generics

import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC

import System.Random
import Control.Monad

import Data.Maybe
import Data.UUID

import Data.Types
import Data.Utils

import qualified Control.Server.Photo as SPhoto
import qualified Control.Logger as Logger

makeHandle :: Logger.Handle IO -> Connection -> SPhoto.Handle IO
makeHandle hl c = SPhoto.Handle
  { SPhoto.hPutPhoto = hPutPhoto hl c
  , SPhoto.hGetPhoto = hGetPhoto hl c
  }

hGetPhoto :: Logger.Handle IO -> Connection -> Photo -> IO (Maybe Base64)
hGetPhoto hl c p' = do
  Logger.logInfo hl "Get photo"
  l <- join . maybeToList <$> traverse 
    (\p-> listStreamingRunSelect c $ lookup_ (_photos photoDB) (primaryKey $ 
      PhotoT 
        { _photoUuid = p
        , _photoData = undefined
        }))
    (fromText p') 
  return (_photoData <$> listToMaybe l)

hPutPhoto :: Logger.Handle IO -> Connection -> Base64 -> IO Photo
hPutPhoto hl c b = do
  Logger.logInfo hl "Put photo"
  u <- randomIO @UUID
  _ <- BPC.runInsert c $ insert (_photos photoDB) $ insertValues
    [ PhotoT u b
    ]
  return $ toText u

data PhotoT f = PhotoT
  { _photoUuid :: Columnar f UUID
  , _photoData :: Columnar f Base64
  } deriving (Generic, Beamable)

-- type PhotoTId = PhotoT Identity
-- type PhotoId = PrimaryKey PhotoT Identity

instance Table PhotoT where
  data PrimaryKey PhotoT f = PhotoId (Columnar f UUID)
    deriving (Generic, Beamable)
  primaryKey = PhotoId . _photoUuid

newtype PhotoDB f = PhotoDB
  { _photos :: f (TableEntity PhotoT)
  } deriving (Generic, Database be)

photoDB :: DatabaseSettings be PhotoDB
photoDB = defaultDbSettings

