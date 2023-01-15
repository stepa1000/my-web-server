{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.News 
  ( News(..)
  , NewsCreate(..)
  ) where

import GHC.Generics

import Data.Vector as V

import Data.Time.Calendar.OrdinalDate  -- .Clock
import Data.Aeson

import Data.Types

-- type PhotoURL = Text

data News = News
  { nameNews :: NameNews
  , loginAuthor :: Login
  , nameAuthor :: Name
  , dateCreationNews :: Day -- UTCTime
  , categoryNews :: Category
  , textNews :: Content
  , photoNews :: Vector Photo
  , publicNews :: FlagPublished
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data NewsCreate = NewsCreate 
  { nameNewsCreate :: NameNews
  , categoryNewsCreate :: Category
  , textNewsCreate :: Content
  , photoNewsCreate :: Vector Photo
  , newPhotoNewsCreate :: Vector Base64 
  , publicNewsCreate :: FlagPublished
  } deriving (Generic, ToJSON, FromJSON)
