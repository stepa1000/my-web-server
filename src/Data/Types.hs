{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Types
  ( NewsCategory,
    OffSet,
    Limit,
    DayAt,
    DayUntil,
    DaySince,
    Login,
    Category,
    NewsName,
    Content,
    Name,
    NameNews,
    Password,
    Photo,
    ForString,
    Base64,
    FlagPublished,
    FlagMakeNews,
    FlagAdmin,
    Logined (..),
    Search (..),
    SortBy (..),
    emptySearch,
  )
where

import Data.Text
-- import Data.ByteString
import Data.Time.Calendar.OrdinalDate
import Data.Tree
import Data.UUID
import GHC.Generics
import Servant.API

type NewsCategory = Tree Category -- is Tree Text

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

type NameNews = Text

type Password = Text

type Photo = Text

type ForString = Text

type Base64 = Text

type FlagPublished = Bool

type FlagMakeNews = Bool

type FlagAdmin = Bool

data Logined = Logined
  { loginLogined :: Login,
    passwordLogined :: Password
  }

emptySearch :: Search
emptySearch =
  Search
    { mDayAtSearch = Nothing,
      mDayUntil = Nothing,
      mDaySince = Nothing,
      mAuthor = Nothing,
      mCategory = Nothing,
      mNewsUUID = Nothing,
      mNewsName = Nothing,
      mContent = Nothing,
      mForString = Nothing,
      mFlagPublished = Nothing,
      mSortBy = Nothing,
      mOffSet = Nothing,
      mLimit = Nothing
    }

-- | Serching for news.
--
-- A lot of data is put in Maybe, it's necessary to
-- be able to diversify the search, i.e. to specify only those fields that are needed.
--
-- Since there are many search parameters, they are placed in a structure, for ease of use.
data Search = Search
  { mDayAtSearch :: Maybe DayAt,
    mDayUntil :: Maybe DayUntil,
    mDaySince :: Maybe DaySince,
    mAuthor :: Maybe Name,
    mCategory :: Maybe Category,
    mNewsUUID :: Maybe UUID,
    mNewsName :: Maybe NewsName,
    mContent :: Maybe Content,
    mForString :: Maybe ForString,
    mFlagPublished :: Maybe FlagPublished,
    mSortBy :: Maybe SortBy,
    mOffSet :: Maybe OffSet,
    mLimit :: Maybe Limit
  }

data SortBy
  = SBDate
  | SBAuthor
  | SBCategory
  | SBCountPhoto
  deriving (Generic)

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
