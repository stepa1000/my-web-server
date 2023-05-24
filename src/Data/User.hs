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

data UserPublic = UserPublic
  { nameUser :: Name,
    loginUser :: Login,
    dateCreationUser :: Day,
    adminUser :: FlagAdmin,
    makeNewsUser :: FlagMakeNews
  }
  deriving (Generic, ToJSON, FromJSON, Eq, Show)

data User = User
  { user :: UserPublic,
    passwordHashUser :: B.ByteString
  }
  deriving (Generic)
