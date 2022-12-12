module Data.User where

import GHC.Generics

import Data.Text
import qualified Data.ByteString as B
import Data.Time.Calendar.OrdinalDate -- Clock

data UserPublic = UserPublic
  { nameUser :: Text
  , loginUser :: Text
  -- , passwordHashUser :: B.ByteString
  , dateCreationUser :: Day -- UTCTime
  , adminUser :: Bool
  , makeNewsUser :: Bool
  } deriving Generic

data User = User
  { user :: UserPublic
  , passwordHashUser :: B.ByteString
  } deriving Generic

type NewsCategory = Tree Text

