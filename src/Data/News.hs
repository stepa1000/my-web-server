{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.News
  ( News (..),
    NewsCreate (..),
    exempleNewsCreate,
  )
where

-- .Clock
import Data.Aeson
import Data.Time.Calendar.OrdinalDate
import Data.Types
import Data.Vector as V
import GHC.Generics

data News = News
  { nameNews :: NameNews,
    loginAuthor :: Login,
    nameAuthor :: Name,
    dateCreationNews :: Day,
    categoryNews :: Category,
    textNews :: Content,
    photoNews :: Vector Photo,
    publicNews :: FlagPublished
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data NewsCreate = NewsCreate
  { nameNewsCreate :: NameNews,
    categoryNewsCreate :: Category,
    textNewsCreate :: Content,
    photoNewsCreate :: Vector Photo,
    newPhotoNewsCreate :: Vector Base64,
    publicNewsCreate :: FlagPublished
  }
  deriving (Generic, ToJSON, FromJSON)

exempleNewsCreate :: NewsCreate
exempleNewsCreate =
  NewsCreate
    { nameNewsCreate = "exepleName",
      categoryNewsCreate = "exempleCategory",
      textNewsCreate = "exempleText",
      photoNewsCreate = V.empty,
      newPhotoNewsCreate = V.empty,
      publicNewsCreate = False
    }
