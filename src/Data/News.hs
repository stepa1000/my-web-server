{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.News where

import GHC.Generics

import Data.Text
import Data.Vector as V
import Data.Tree as T
import qualified Data.ByteString as B

import Data.Time.Calendar.OrdinalDate  -- .Clock
import Data.Aeson

import Data.Types

type NewsCategory = Tree Text

type PhotoURL = Text

data News = News
  { nameNews :: NameNews
  , loginAothor :: Login
  , nameAothor :: Name
  , dateCreationNews :: Day -- UTCTime
  , categoryNews :: Category
  , textNews :: Content
  , photoNews :: Vector PhotoURL
  , publicNews :: FlagPublished
  } deriving (Generic, ToJSON, FromJSON)

data NewsCreate = NewsCreate 
  { nameNewsCreate :: NameNews -- UTCTime
  , categoryNewsCreate :: Category
  , textNewsCreate :: Content
  , photoNewsCreate :: Vector PhotoURL
  , publicNewsCreate :: FlagPublished
  } deriving (Generic, ToJSON)
