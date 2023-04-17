{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Imp.Database
  ( webServerDB,
    UserT (..),
    NewsT (..),
    PhotoT (..),
    WebServerDB (..),
    UserTId,
    NewsTId,
    UserId,
    CategoryTId,
    CategoryT (..),
    categoryName,
  )
where

import Data.ByteString
import Data.Time.Calendar.OrdinalDate
import Data.Types
import Data.UUID
import Data.Vector
import Database.Beam as Beam
import Prelude as P

data UserT f = UserT
  { _userName :: Columnar f Name,
    _userLogin :: Columnar f Login,
    _userPasswordHash :: Columnar f ByteString,
    _userDateCreation :: Columnar f Day,
    _userAdmin :: Columnar f FlagAdmin,
    _userMakeNews :: Columnar f FlagMakeNews
  }
  deriving (Generic, Beamable)

type UserTId = UserT Identity

type UserId = PrimaryKey UserT Identity

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Login)
    deriving (Generic, Beamable)
  primaryKey = UserId . _userLogin

data NewsT f = NewsT
  { _newsNewsName :: Columnar f NameNews,
    _newsLoginAuthor :: Columnar f Login,
    _newsNameAuthor :: Columnar f Name,
    _newsDateCreation :: Columnar f Day,
    _newsCategory :: Columnar f Category,
    _newsContent :: Columnar f Content,
    _newsPhoto :: Columnar f ByteString,
    _newsPublic :: Columnar f FlagPublished
  }
  deriving (Generic, Beamable)

type NewsTId = NewsT Identity

instance Table NewsT where
  data PrimaryKey NewsT f = NewsId (Columnar f NameNews)
    deriving (Generic, Beamable)
  primaryKey = NewsId . _newsNewsName

data CategoryT f = CategoryT
  { _categoryCategoryName :: Columnar f Category,
    _categoryParent :: Columnar f Category,
    _categoryChild :: Columnar f (Vector Category)
  }
  deriving (Generic, Beamable)

categoryName :: Category -> CategoryTId
categoryName c =
  CategoryT
    { _categoryCategoryName = c,
      _categoryParent = undefined,
      _categoryChild = undefined
    }

type CategoryTId = CategoryT Identity

instance Table CategoryT where
  data PrimaryKey CategoryT f = CategoryId (Columnar f Category)
    deriving (Generic, Beamable)
  primaryKey = CategoryId . _categoryCategoryName

data PhotoT f = PhotoT
  { _photoUuidPhoto :: Columnar f UUID,
    _photoData :: Columnar f Base64
  }
  deriving (Generic, Beamable)

instance Table PhotoT where
  data PrimaryKey PhotoT f = PhotoId (Columnar f UUID)
    deriving (Generic, Beamable)
  primaryKey = PhotoId . _photoUuidPhoto

data WebServerDB f = WebServerDB
  { dbUser :: f (TableEntity UserT),
    dbNews :: f (TableEntity NewsT),
    dbPhoto :: f (TableEntity PhotoT),
    dbCategory :: f (TableEntity CategoryT)
  }
  deriving (Generic)
  deriving anyclass (Database be)

webServerDB :: DatabaseSettings be WebServerDB
webServerDB = defaultDbSettings
