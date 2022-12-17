{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies#-}

module Data.User where

import GHC.Generics

import Database.Beam
import Database.Beam.Postgres

import Data.Text
import qualified Data.ByteString as B
import Data.Time.Calendar.OrdinalDate -- Clock
import Data.Aeson

import Data.Functor.Identity

import Data.Types

data UserPublic = UserPublic
  { nameUser :: Text
  , loginUser :: Text
  -- , passwordHashUser :: B.ByteString
  , dateCreationUser :: Day -- UTCTime
  , adminUser :: FlagAdmin
  , makeNewsUser :: FlagMakeNews
  } deriving (Generic,ToJSON,FromJSON)

data User = User
  { user :: UserPublic
  , passwordHashUser :: Password -- B.ByteString
  } deriving Generic

data UserT f = UserT
  { _nameUserT :: Columnar f Name
  , _loginUserT :: Columnar f Login
  , _passwordUserT :: Columnar f Password
  , _dateCreationUserT :: Columnar f Day
  , _adminUserT :: Columnar f FlagAdmin
  , _makeNewsUserT :: Columnar f FlagMakeNews
  } deriving (Generic, Beamable)

type UserTId = UserT Identity
type UserId = PrimaryKey UserT Identity

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Login) 
    deriving (Generic, Beamable)
  primaryKey = UserId . _loginUserT

data AccountDB f = AccountDB
  { _accounts :: f (TableEntity UserT) } 
  deriving (Generic, Database be)

accountDB :: DatabaseSettings be AccountDB
accountDB = defaultDbSettings
