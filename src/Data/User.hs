{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Data.User
  ( UserPublic (..),
    User (..),
  )
where

-- import Data.Text

-- Clock
import Data.Aeson
import qualified Data.ByteString as B
import Data.Time.Calendar.OrdinalDate
import Data.Types
import GHC.Generics

-- | The public part is available to any user.
--
-- Stores identifying data and fields to control
-- the availability of functionality.
data UserPublic = UserPublic
  { -- | Name for the user. Can match other names. For self-implementation.
    nameUser :: Name,
    -- | For identification during authorization
    -- and search by user request. Must be unique.
    loginUser :: Login,
    -- | Inofrmation for self-affirmation at the expense of new users.
    dateCreationUser :: Day,
    -- | Can the user create/delete catigories, create users.
    adminUser :: FlagAdmin,
    -- | Can the user create news.
    makeNewsUser :: FlagMakeNews
  }
  deriving (Generic, ToJSON, FromJSON, Eq, Show)

-- | The all data of user.
--
-- Contains a private field with a threshold.
data User = User
  { user :: UserPublic,
    passwordHashUser :: B.ByteString
  }
  deriving (Generic)
