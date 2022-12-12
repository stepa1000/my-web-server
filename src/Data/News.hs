module Data.News where

import GHC.Generics

import Data.Text
import qualified Data.ByteString as B
import Data.Time.Calendar.OrdinalDate  -- .Clock

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
  } deriving Generic
