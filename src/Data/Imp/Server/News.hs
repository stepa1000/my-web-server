{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wwarn #-}

module Data.Imp.Server.News 
  ( Config(..)
  , makeHandle
  , NewsT(..) -- ????
  , NewsDB(..)
  , newsDB
--  , hSearchNewsName -- !!!!!!!!
  , hSearchContent -- !!!!!!
  , debugPosition -- !!!!
  ) where

import Prelude as P

import GHC.Generics

import Database.Beam as Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Database.Beam.Query.Internal

import Data.ByteString

import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock

import Data.Maybe as Maybe
import Data.Vector as V

import Data.Aeson as A
import Data.Yaml as Y
import Data.List (sortBy)
import Data.String

import Data.News
import Data.Types
import Data.Utils

import qualified Data.Imp.Server.Photo as ImpSPhoto
import qualified Control.Server.News as SNews

newtype Config = Config
  {confMaxLimit :: Integer} 
  deriving (Generic, Y.ToJSON, Y.FromJSON)

makeHandle :: Config -> Connection -> SNews.Handle IO
makeHandle conf c = SNews.Handle 
  { SNews.handlePhoto = ImpSPhoto.makeHandle c
  , SNews.hSearchNews = hSearchNews (confMaxLimit conf) c
  , SNews.hPutNews = hPutNews c
  , SNews.hGetNews = hGetNews c
  , SNews.hModifNews = hModifNews c
  , SNews.hGetDay = hGetDay
  }

hSearchNews :: Integer -> Connection -> Search -> IO [News]
hSearchNews maxLimit c s = do
  lm <- (fmap . fmap) newsTToNews $ listStreamingRunSelect c $ select $ searchNews (toInteger <$> mLimit s) (toInteger <$> mOffSet s)
  return $ sortNews (mSortBy s) $ Maybe.catMaybes lm
  where 
    searchNews (Just l) (Just o) = limit_ (min maxLimit l) $ offset_ o $ filter_ (filterSearch s) (all_ (_news newsDB)) 
    searchNews _ _ = limit_ maxLimit $ offset_ 0 $ filter_ (filterSearch s) (all_ (_news newsDB))

hSearchContent :: Integer -> Connection -> Content -> IO [News]
hSearchContent maxLimit c content =  do
  lm <- (fmap . fmap) newsTToNews $ listStreamingRunSelect c $ select $ searchNews Nothing Nothing
  return $ Maybe.catMaybes lm
  where 
    searchNews (Just l) (Just o) = limit_ (min maxLimit l) $ offset_ o $ filter_' (filterContent' (Just content)) (all_ (_news newsDB)) 
    searchNews _ _ = limit_ maxLimit $ offset_ 0 $ filter_' (filterContent' (Just content)) (all_ (_news newsDB))

debugPosition :: Connection -> Content -> IO ByteString 
debugPosition c content = do
  pgTraceStmtIO' @(SqlSelect Postgres Integer)  c $ select $ do
    n <- all_ (_news newsDB)
    return $ position (val_ content) (_newsContent n)

position :: QGenExpr QValueContext Postgres QBaseScope Content -> QGenExpr QValueContext Postgres QBaseScope Content -> QGenExpr QValueContext Postgres QBaseScope Integer
position = customExpr_ f
  where
    f :: (Monoid a, IsString a) => a -> a -> a
    f c1 c2 = "position(" <> c1 <> " IN " <> c2 <> ")"

positionQNested :: QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Content 
                -> QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Content 
                -> QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Integer
positionQNested = customExpr_ f
  where
    f :: (Monoid a, IsString a) => a -> a -> a
    f c1 c2 = "position(" <> c1 <> " IN " <> c2 <> ")"

sortNews :: Maybe SortBy -> [News] -> [News]
sortNews (Just SBDate) = sortBy (\a b-> compare (dateCreationNews a) (dateCreationNews b) )
sortNews (Just SBAuthor) = sortBy (\a b-> compare (nameAuthor a) (nameAuthor b))
sortNews (Just SBCategory) = sortBy (\a b-> compare (categoryNews a) (categoryNews b))
sortNews (Just SBCountPhoto) = sortBy (\a b-> compare (V.length $ photoNews a) (V.length $ photoNews b))
sortNews Nothing = id

filterSearch (Search mDayAt' mDayUntil' mDaySince' mAuthor' mCategory' mNewsNam' mContent' mForString' mFlagPublished' _ _ _) n =
  filterDaySince mDaySince' n &&.
  filterDayAt mDayAt' n &&.
  filterDayUntil mDayUntil' n &&.
  filterAuthor mAuthor' n &&.
  filterCategory mCategory' n &&.
  filterNewsName mNewsNam' n &&.
  filterFlagPublished mFlagPublished' n &&.
  filterContent mContent' n &&.
  f mForString' 
  where
    f (Just fs) = 
      filterForStringName fs n ||. 
      filterForStringContent fs n ||. 
      filerForStringAuthor fs n ||. 
      filterForStringCategory fs n
    f Nothing = val_ True

filterForStringContent fs n = positionQNested (val_ fs) (_newsContent n) /=. val_ 0
filterForStringName s n = positionQNested (val_ s) (_newsNewsName n) /=. val_ 0
filerForStringAuthor s n = positionQNested (val_ s) (_newsNameAuthor n) /=. val_ 0
filterForStringCategory s n = positionQNested (val_ s) (_newsCategory n) /=. val_ 0

filterContent (Just c) n = positionQNested (val_ c) (_newsContent n) /=. val_  0
filterContent Nothing _ = val_ True

filterContent' (Just c) n = positionQNested (val_ c) (_newsContent n) /=?. val_ 0
filterContent' Nothing _ = sqlBool_ $ val_ True

-- filterFlagPublished :: Maybe FlagPublished -> NewsT (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool
filterFlagPublished :: (HaskellLiteralForQExpr (expr Bool) ~ Bool,
                       SqlEq expr (Columnar f FlagPublished), SqlValable (expr Bool),
                       SqlValable (Columnar f FlagPublished)) 
                       => Maybe (HaskellLiteralForQExpr (Columnar f FlagPublished))
                       -> NewsT f -> expr Bool
filterFlagPublished (Just fp) n = _newsPublic n ==. val_ fp
filterFlagPublished Nothing _ = val_ True
    
-- filterNewsName :: Maybe NewsName -> NewsT (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool
filterNewsName :: (HaskellLiteralForQExpr (expr Bool) ~ Bool,
                  SqlEq expr (Columnar f NameNews), SqlValable (expr Bool),
                  SqlValable (Columnar f NameNews)) 
                  => Maybe (HaskellLiteralForQExpr (Columnar f NameNews))
                  -> NewsT f -> expr Bool
filterNewsName (Just nn) n = _newsNewsName n ==. val_ nn
filterNewsName Nothing _ = val_ True

-- filterCategory :: Maybe Category -> NewsT (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool
filterCategory :: (HaskellLiteralForQExpr (expr Bool) ~ Bool,
                  SqlEq expr (Columnar f Category), SqlValable (expr Bool),
                  SqlValable (Columnar f Category)) 
                  => Maybe (HaskellLiteralForQExpr (Columnar f Category))
                  -> NewsT f -> expr Bool
filterCategory (Just c) n = _newsCategory n ==. val_ c
filterCategory Nothing _ = val_ True

-- filterAuthor :: Maybe Name -> NewsT (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool
filterAuthor :: (HaskellLiteralForQExpr (expr Bool) ~ Bool,
                SqlEq expr (Columnar f Name), SqlValable (expr Bool),
                SqlValable (Columnar f Name)) 
                => Maybe (HaskellLiteralForQExpr (Columnar f Name))
                -> NewsT f -> expr Bool
filterAuthor (Just a) n = _newsNameAuthor n ==. val_ a
filterAuthor Nothing _ = val_ True

-- filterDaySince :: Maybe DaySince -> NewsT (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool
filterDaySince :: (HaskellLiteralForQExpr (expr Bool) ~ Bool,
                  SqlEq expr (Columnar f Day), SqlValable (expr Bool),
                  SqlValable (Columnar f Day)) 
                  => Maybe (HaskellLiteralForQExpr (Columnar f Day))
                  -> NewsT f -> expr Bool
filterDaySince (Just ds) n = _newsDateCreation n ==. val_ ds 
filterDaySince Nothing _ = val_ True

-- filterDayAt :: Maybe DayAt -> NewsT (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool
filterDayAt :: (HaskellLiteralForQExpr (expr Bool) ~ Bool,
               SqlOrd expr (Columnar f Day), SqlValable (expr Bool),
               SqlValable (Columnar f Day)) 
               => Maybe (HaskellLiteralForQExpr (Columnar f Day))
               -> NewsT f -> expr Bool
filterDayAt (Just dat) n = _newsDateCreation n >=. val_ dat 
filterDayAt Nothing _ = val_ True

-- filterDayUntil :: Maybe DayUntil -> NewsT (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool
filterDayUntil :: (HaskellLiteralForQExpr (expr Bool) ~ Bool,
                  SqlOrd expr (Columnar f Day), SqlValable (expr Bool),
                  SqlValable (Columnar f Day)) 
                  => Maybe (HaskellLiteralForQExpr (Columnar f Day))
                  -> NewsT f -> expr Bool
filterDayUntil (Just dat) n = _newsDateCreation n <=. val_ dat 
filterDayUntil Nothing _ = val_ True

hGetDay :: IO Day
hGetDay = do
  (UTCTime d _) <- getCurrentTime
  return d

hModifNews :: Connection -> NameNews -> (News -> News) -> IO ()
hModifNews c nn f = do
  l <- listStreamingRunSelect c $ lookup_ (_news newsDB) (primaryKey $ nameNewsT nn)
  case l of
    (x:_) -> do
      _ <- BPC.runDelete c $ delete (_news newsDB)
        (\n-> _newsNewsName n ==. val_ (_newsNewsName x) )
      let mn = f <$> newsTToNews x
      traverse_ (\n-> do
          BPC.runInsert c $ Beam.insert (_news newsDB) $ insertValues
            [ newsToNewsT n
            ]
        ) mn
    [] -> return ()

hGetNews :: Connection -> NewsName -> IO (Maybe News)
hGetNews c nn = do
  l <- listStreamingRunSelect c $ lookup_ (_news newsDB) (primaryKey $ nameNewsT nn)
  case l of
    (x:_) -> return $ newsTToNews x
    [] -> return Nothing
      

hPutNews :: Connection -> News -> IO ()
hPutNews c n = do
  _ <- BPC.runInsert c $ Beam.insert (_news newsDB) $ insertValues
    [ newsToNewsT n
    ]
  return ()

newsTToNews :: NewsTId -> Maybe News
newsTToNews n = do
  v <- A.decode $ fromStrict $ _newsPhoto n
  return $ News
    { nameNews = _newsNewsName n
    , loginAuthor = _newsLoginAuthor n
    , nameAuthor = _newsNameAuthor n
    , dateCreationNews = _newsDateCreation n
    , categoryNews = _newsCategory n
    , textNews = _newsContent n
    , photoNews = v
    , publicNews = _newsPublic n
    }

newsToNewsT :: News -> NewsTId
newsToNewsT n = NewsT
  { _newsNewsName = nameNews n
  , _newsLoginAuthor = loginAuthor n
  , _newsNameAuthor = nameAuthor n
  , _newsDateCreation = dateCreationNews n
  , _newsCategory = categoryNews n
  , _newsContent = textNews n
  , _newsPhoto = toStrict $ A.encode $ photoNews n
  , _newsPublic = publicNews n
  }

nameNewsT :: NameNews -> NewsTId
nameNewsT nn = NewsT 
    { _newsNewsName = nn
    , _newsLoginAuthor = undefined
    , _newsNameAuthor = undefined
    , _newsDateCreation = undefined
    , _newsCategory = undefined
    , _newsContent = undefined
    , _newsPhoto = undefined -- PhotoVector-- [Photo] -- (Vector Photo)
    , _newsPublic = undefined
    }

data NewsT f = NewsT
  { _newsNewsName :: Columnar f NameNews
  , _newsLoginAuthor :: Columnar f Login
  , _newsNameAuthor :: Columnar f Name
  , _newsDateCreation :: Columnar f Day
  , _newsCategory :: Columnar f Category
  , _newsContent :: Columnar f Content
  , _newsPhoto :: Columnar f ByteString-- PhotoVector-- [Photo] -- (Vector Photo)
  , _newsPublic :: Columnar f FlagPublished
  } deriving (Generic, Beamable)

type NewsTId = NewsT Identity
-- type NewsId = PrimaryKey NewsT Identity 

instance Table NewsT where
  data PrimaryKey NewsT f = NewsId (Columnar f NameNews)
    deriving (Generic, Beamable)
  primaryKey = NewsId . _newsNewsName

newtype NewsDB f = NewsDB
  { _news :: f (TableEntity NewsT) }
  deriving (Generic, Database be)

newsDB :: DatabaseSettings be NewsDB
newsDB = defaultDbSettings

