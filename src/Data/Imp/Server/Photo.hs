{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Imp.Server.Photo where

import Prelude as P

import GHC.Generics

import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Conduit

import System.Random
import Control.Applicative
import Control.Monad.Catch

import Data.Text
import Data.ByteString
import Data.Binary
import Data.ByteArray

import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Crypto.Hash
import Crypto.Hash.IO

import Data.Maybe
import Data.Typeable
import Data.UUID

import Data.News
import Data.User
import Data.Types

import qualified Control.Server.Photo as SPhoto

hGetPhoto :: Photo -> IO (Maybe Base64)
hGetPhoto p = do
  l <- listStreamingRunSelect c $ lookup_ (_photos photoDB) (primaryKey $ PhotoT {_photoUuid = p})
  return $ listToMaybe l

hPutPhoto :: Base64 -> IO Photo
hPutPhoto b = do
  u <- randomIO @UUID
  BPC.runInsert c $ insert (_photos photoDB) $ insertValues
    [ PhotoT u b
    ]
  return $ toText u

data PhotoT f = PhotoT
  { _photoUuid :: Columnar f UUID
  , _photoData :: Columnar f Base64
  } deriving (Generic, Beamable)

type PhotoTId = PhotoT Identity
type PhotoId = PrimaryKey UserT Identity

instance Table PhotoT where
  data PrimaryKey PhotoT f = PhotoId (Columnar f Text)
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
