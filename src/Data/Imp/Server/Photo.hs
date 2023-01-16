{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

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

makeHandle :: Connection -> SPhoto.Handle IO
makeHandle c = SPhoto.Handle
  { SPhoto.hPutPhoto = hPutPhoto c
  , SPhoto.hGetPhoto = hGetPhoto c
  }

hGetPhoto :: Connection -> Photo -> IO (Maybe Base64)
hGetPhoto c p' = do
  l <- join . maybeToList <$> traverse 
    (\p-> listStreamingRunSelect c $ lookup_ (_photos photoDB) (primaryKey $ 
      PhotoT 
        { _photoUuid = p
        , _photoData = undefined
        }))
    (fromText p') 
  return (_photoData <$> listToMaybe l)

hPutPhoto :: Connection -> Base64 -> IO Photo
hPutPhoto c b = do
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

