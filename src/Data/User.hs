{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies#-}

module Data.User
  ( UserPublic(..)
  , User (..)
  ) where

import GHC.Generics

--import Data.Text
import qualified Data.ByteString as B
import Data.Time.Calendar.OrdinalDate -- Clock
import Data.Aeson

import Data.Types

data UserPublic = UserPublic
  { nameUser :: Name
  , loginUser :: Login
  -- , passwordHashUser :: B.ByteString
  , dateCreationUser :: Day -- UTCTime
  , adminUser :: FlagAdmin
  , makeNewsUser :: FlagMakeNews
  } deriving (Generic,ToJSON,FromJSON,Eq,Show)

data User = User
  { user :: UserPublic
  , passwordHashUser :: B.ByteString
  } deriving Generic

