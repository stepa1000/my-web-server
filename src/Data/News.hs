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

type NewsCategory = Tree Text

type PhotoURL = Text

data News = News
  { nameNews :: Text
  , loginAothor :: Text
  , nameAothor :: Text
  , dateCreationNews :: Day -- UTCTime
  , categoryNews :: Text
  , textNews :: Text
  , photoNews :: Vector PhotoURL
  , publicNews :: Bool
  } deriving (Generic, ToJSON, FromJSON)

data NewsCreate = NewsCreate 
  { nameNewsCreate :: Text -- UTCTime
  , categoryNewsCreate :: Text
  , textNewsCreate :: Text
  , photoNewsCreate :: Vector PhotoURL
  , publicNewsCreate :: Bool
  } deriving (Generic, ToJSON)
