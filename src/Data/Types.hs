{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Types where

import GHC.Generics

import Servant.API

import Data.Text
import Data.Time.Calendar.OrdinalDate

type OffSet = Int
type Limit = Int

type DayAt = Day
type DayUntil = Day
type DaySince = Day

type Login = Text
type Category = Text
type NewsName = Text
type Content = Text

type Name = Text
-- type Authot = Text
type NameNews = Text
type Password = Text

type Photo = Text
type ForString = Text
type Base64 = Text

type FlagPublished = Bool
type FlagMakeNews = Bool
type FlagAdmin = Bool

data Logined = Logined
  { loginLogined :: Login
  , passwordLogined :: Password
  }

data Search = Search
  { mDayAtSerch :: Maybe DayAt
  , mDayUntil :: Maybe DayUntil
  , mDaySince :: Maybe DaySince
  , mName :: Maybe Name
  , mCategory :: Maybe Category
  , mNewsName :: Maybe NewsName
  , mContent :: Maybe Content
  , mForString :: Maybe ForString
  , mFlagPublished :: Maybe FlagPublished
  , mSortBy :: Maybe SortBy
  , mOffSet :: Maybe OffSet
  , mLimit :: Maybe Limit
  }

data SortBy
  = SBDate           
  | SBAuthor
  | SBCategory
  | SBCountPhoto
  deriving Generic

instance ToHttpApiData SortBy where
  toUrlPiece SBDate = "date"
  toUrlPiece SBAuthor = "author"
  toUrlPiece SBCategory = "category"
  toUrlPiece SBCountPhoto = "photo"
