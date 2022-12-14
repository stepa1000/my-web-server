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
-- import Conduit

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
  l <- fmap (join . maybeToList) $ traverse 
    (\p-> listStreamingRunSelect c $ lookup_ (_photos photoDB) (primaryKey $ 
      PhotoT 
        { _photoUuid = p
        , _photoData = undefined
        }))
    (fromText p') 
  return $ fmap _photoData $ listToMaybe l

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

data PhotoDB f = PhotoDB
  { _photos :: f (TableEntity PhotoT)
  } deriving (Generic, Database be)

photoDB :: DatabaseSettings be PhotoDB
photoDB = defaultDbSettings
{-
data UserT f = UserT
  { _userName :: Columnar f Name
  , _userLogin :: Columnar f Login
  , _userPasswordHash :: Columnar f ByteString
  , _userDateCreation :: Columnar f Day
  , _userAdmin :: Columnar f FlagAdmin
  , _userMakeNews :: Columnar f FlagMakeNews
  } deriving (Generic, Beamable)

type UserTId = UserT Identity
type UserId = PrimaryKey UserT Identity

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Login) 
    deriving (Generic, Beamable)
  primaryKey = UserId . _userLogin

data AccountDB f = AccountDB
  { _accounts :: f (TableEntity UserT) } 
  deriving (Generic, Database be)

accountDB :: DatabaseSettings be AccountDB
accountDB = defaultDbSettings 
-}
