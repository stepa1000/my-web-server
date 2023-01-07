{-# LANGUAGE DeriveGeneric,
OverloadedStrings #-}

module Data.Types 
  ( NewsCategory
  , OffSet
  , Limit
  , DayAt
  , DayUntil
  , DaySince
  , Login
  , Category
  , NewsName
  , Content
  , Name
  , NameNews
  , Password
  , Photo
  , ForString
  , Base64
  , FlagPublished
  , FlagMakeNews
  , FlagAdmin
  , Logined(..)
  , Search(..)
  , SortBy(..)
  , emptySearch
  ) where

import GHC.Generics

import Servant.API

import Data.Text
import Data.Tree
-- import Data.ByteString
import Data.Time.Calendar.OrdinalDate

type NewsCategory = Tree Category -- Text

type OffSet = Int
type Limit = Int

type DayAt = Day
type DayUntil = Day
type DaySince = Day

type Login = Text -- ByteString
type Category = Text
type NewsName = Text
type Content = Text

type Name = Text
-- type Authot = Text
type NameNews = Text
type Password = Text -- ByteString

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

emptySearch :: Search
emptySearch = Search
  { mDayAtSerch = Nothing
  , mDayUntil = Nothing
  , mDaySince = Nothing
  , mName = Nothing
  , mCategory = Nothing
  , mNewsName = Nothing
  , mContent = Nothing
  , mForString = Nothing
  , mFlagPublished = Nothing
  , mSortBy = Nothing
  , mOffSet = Nothing
  , mLimit = Nothing
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

instance FromHttpApiData SortBy where
  parseUrlPiece "date" = return SBDate
  parseUrlPiece "author" = return SBAuthor
  parseUrlPiece "category" = return SBCategory
  parseUrlPiece "photo" = return SBCountPhoto
  parseUrlPiece _ = Left "dont parse SortBy"
