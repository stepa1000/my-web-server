{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.User where

import GHC.Generics

import Data.Text
import qualified Data.ByteString as B
import Data.Time.Calendar.OrdinalDate -- Clock
import Data.Aeson

data UserPublic = UserPublic
  { nameUser :: Text
  , loginUser :: Text
  -- , passwordHashUser :: B.ByteString
  , dateCreationUser :: Day -- UTCTime
  , adminUser :: Bool
  , makeNewsUser :: Bool
  } deriving (Generic,ToJSON,FromJSON)

data User = User
  { user :: UserPublic
  , passwordHashUser :: B.ByteString
  } deriving Generic


