{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies#-}

module Data.User where

import GHC.Generics

-- import Database.Beam
-- import Database.Beam.Postgres

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
  } deriving (Generic,ToJSON,FromJSON,Eq,Show)

data User = User
  { user :: UserPublic
  , passwordHashUser :: B.ByteString
  } deriving Generic

