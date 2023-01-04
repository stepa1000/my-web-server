{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Imp.Server.News 
  ( Config(..)
  , makeHandle
  ) where

import Prelude as P

import GHC.Generics

import Database.Beam as Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
--import Conduit

import Database.Beam.Backend.SQL -- .BeamSqlBackendSynte
import Database.Beam.Backend.SQL.SQL92

--import System.Random
--import Control.Applicative
--import Control.Monad.Catch
--import Control.Monad

--import Data.Text
import Data.ByteString
--import Data.Binary
--import Data.ByteArray

import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
--import Crypto.Hash
--import Crypto.Hash.IO

import Data.Maybe as Maybe
--import Data.Typeable
--import Data.UUID
import Data.Vector as V

import Data.Aeson as A
import Data.List (sortBy)

import Data.News
-- import Data.User
import Data.Types
import Data.Utils

-- import qualified Control.Server.Photo as SPhoto
import qualified Data.Imp.Server.Photo as ImpSPhoto
import qualified Control.Server.News as SNews

data Config = Config
  {confMaxLimit :: Integer}

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
    -- searchNews Nothing (Just o) = offset_ o $ filter_ (filterSearch s) (all_ $ (_news newsDB))orderBy_ (funOrder $ mSortBy s)
    -- searchNews (Just l) Nothing = limit_ l $ filter_ (filterSearch s) (all_ $ (_news newsDB)) -- -}
    searchNews _ _ = limit_ (maxLimit) $ offset_ 0 $ filter_ (filterSearch s) (all_ (_news newsDB))

sortNews :: Maybe SortBy -> [News] -> [News]
sortNews (Just SBDate) = sortBy (\a b-> compare (dateCreationNews a) (dateCreationNews b) )
sortNews (Just SBAuthor) = sortBy (\a b-> compare (nameAuthor a) (nameAuthor b))
sortNews (Just SBCategory) = sortBy (\a b-> compare (categoryNews a) (categoryNews b))
sortNews (Just SBCountPhoto) = sortBy (\a b-> compare (V.length $ photoNews a) (V.length $ photoNews b))
sortNews Nothing = id

-- filterSearch :: Search -> NewsT (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool
filterSearch :: (Columnar f NameNews
                       ~ QGenExpr QValueContext be s ForString,
                       HaskellLiteralForQExpr (Columnar f Day) ~ Day,
                       -- HaskellLiteralForQExpr (Columnar f Name) ~ Text,
                       HaskellLiteralForQExpr (Columnar f FlagPublished) ~ Bool,
                       BeamSqlBackendIsString be ForString,
                       SqlOrd (QGenExpr QValueContext be s) (Columnar f Day),
                       HasSqlEqualityCheck be Integer,
                       SqlEq (QGenExpr QValueContext be s) (Columnar f Day),
                       SqlEq (QGenExpr QValueContext be s) (Columnar f NameNews),
                       SqlEq (QGenExpr QValueContext be s) (Columnar f FlagPublished),
                       SqlValable (Columnar f Day), SqlValable (Columnar f NameNews),
                       SqlValable (Columnar f FlagPublished),
                       HasSqlValueSyntax
                         (Sql92ExpressionValueSyntax
                            (Sql92SelectTableExpressionSyntax
                               (Sql92SelectSelectTableSyntax
                                  (Sql92SelectSyntax (BeamSqlBackendSyntax be)))))
                         ForString,
                       HasSqlValueSyntax
                         (Sql92ExpressionValueSyntax
                            (Sql92SelectTableExpressionSyntax
                               (Sql92SelectSelectTableSyntax
                                  (Sql92SelectSyntax (BeamSqlBackendSyntax be)))))
                         Integer) =>
                      Search -> NewsT f -> QGenExpr QValueContext be s Bool
filterSearch (Search mDayAt' mDayUntil' mDaySince' mAuthor' mCategory' mNewsNam' mContent' mForString' mFlagPublished' _ _ _) n =
  (filterDaySince mDaySince' n) &&.
  (filterDayAt mDayAt' n) &&.
  (filterDayUntil mDayUntil' n) &&.
  (filterAuthor mAuthor' n) &&.
  (filterCategory mCategory' n) &&.
  (filterNewsName mNewsNam' n) &&.
  (filterFlagPublished mFlagPublished' n) &&.
  (filterContent mContent' n) &&.
  (f mForString') -- (filterForStringName mForString n) (filterForStringContent mForString n ))
  where
    f (Just fs) = filterForStringName fs n ||. filterForStringContent fs n
    f Nothing = val_ True

filterForStringContent :: (Columnar f Content
                                 ~ QGenExpr QValueContext w1 s w2,
                                 HasSqlEqualityCheck w1 Integer,
                                 Database.Beam.Backend.SQL.BeamSqlBackendIsString w1 w2,
                                 Database.Beam.Backend.SQL.SQL92.HasSqlValueSyntax
                                   (Database.Beam.Backend.SQL.SQL92.Sql92ExpressionValueSyntax
                                      (Database.Beam.Backend.SQL.SQL92.Sql92SelectTableExpressionSyntax
                                         (Database.Beam.Backend.SQL.SQL92.Sql92SelectSelectTableSyntax
                                            (Database.Beam.Backend.SQL.SQL92.Sql92SelectSyntax
                                               (Database.Beam.Backend.SQL.BeamSqlBackendSyntax
                                                  w1)))))
                                   w2,
                                 Database.Beam.Backend.SQL.SQL92.HasSqlValueSyntax
                                   (Database.Beam.Backend.SQL.SQL92.Sql92ExpressionValueSyntax
                                      (Database.Beam.Backend.SQL.SQL92.Sql92SelectTableExpressionSyntax
                                         (Database.Beam.Backend.SQL.SQL92.Sql92SelectSelectTableSyntax
                                            (Database.Beam.Backend.SQL.SQL92.Sql92SelectSyntax
                                               (Database.Beam.Backend.SQL.BeamSqlBackendSyntax
                                                  w1)))))
                                   Integer) =>
                                w2 -> NewsT f -> QGenExpr QValueContext w1 s Bool
filterForStringContent fs n = (position_ @_ @_ @Integer (val_ fs) (_newsContent n)) /=. (val_ 0)

filterForStringName :: (Columnar f NameNews
                              ~ QGenExpr QValueContext w1 s w2,
                              HasSqlEqualityCheck w1 Integer,
                              Database.Beam.Backend.SQL.BeamSqlBackendIsString w1 w2,
                              Database.Beam.Backend.SQL.SQL92.HasSqlValueSyntax
                                (Database.Beam.Backend.SQL.SQL92.Sql92ExpressionValueSyntax
                                   (Database.Beam.Backend.SQL.SQL92.Sql92SelectTableExpressionSyntax
                                      (Database.Beam.Backend.SQL.SQL92.Sql92SelectSelectTableSyntax
                                         (Database.Beam.Backend.SQL.SQL92.Sql92SelectSyntax
                                            (Database.Beam.Backend.SQL.BeamSqlBackendSyntax w1)))))
                                w2,
                              Database.Beam.Backend.SQL.SQL92.HasSqlValueSyntax
                                (Database.Beam.Backend.SQL.SQL92.Sql92ExpressionValueSyntax
                                   (Database.Beam.Backend.SQL.SQL92.Sql92SelectTableExpressionSyntax
                                      (Database.Beam.Backend.SQL.SQL92.Sql92SelectSelectTableSyntax
                                         (Database.Beam.Backend.SQL.SQL92.Sql92SelectSyntax
                                            (Database.Beam.Backend.SQL.BeamSqlBackendSyntax w1)))))
                                Integer) =>
                             w2 -> NewsT f -> QGenExpr QValueContext w1 s Bool
filterForStringName s n = (position_ @_ @_ @Integer (val_ s) (_newsName n)) /=. (val_ 0)
--filterForStringName Nothing _ = val_ True

filterContent :: (Columnar f Content
                        ~ QGenExpr QValueContext w1 s w2,
                        HasSqlEqualityCheck w1 Integer,
                        Database.Beam.Backend.SQL.BeamSqlBackendIsString w1 w2,
                        Database.Beam.Backend.SQL.SQL92.HasSqlValueSyntax
                          (Database.Beam.Backend.SQL.SQL92.Sql92ExpressionValueSyntax
                             (Database.Beam.Backend.SQL.SQL92.Sql92SelectTableExpressionSyntax
                                (Database.Beam.Backend.SQL.SQL92.Sql92SelectSelectTableSyntax
                                   (Database.Beam.Backend.SQL.SQL92.Sql92SelectSyntax
                                      (Database.Beam.Backend.SQL.BeamSqlBackendSyntax w1)))))
                          w2,
                        Database.Beam.Backend.SQL.SQL92.HasSqlValueSyntax
                          (Database.Beam.Backend.SQL.SQL92.Sql92ExpressionValueSyntax
                             (Database.Beam.Backend.SQL.SQL92.Sql92SelectTableExpressionSyntax
                                (Database.Beam.Backend.SQL.SQL92.Sql92SelectSelectTableSyntax
                                   (Database.Beam.Backend.SQL.SQL92.Sql92SelectSyntax
                                      (Database.Beam.Backend.SQL.BeamSqlBackendSyntax w1)))))
                          Integer) =>
                       Maybe w2 -> NewsT f -> QGenExpr QValueContext w1 s Bool
filterContent (Just c) n = (position_ @_ @_ @Integer (val_ c) (_newsContent n)) /=. (val_ 0)
filterContent Nothing _ = val_ True

-- filterFlagPublished :: Maybe FlagPublished -> NewsT (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool
filterFlagPublished :: (HaskellLiteralForQExpr (expr Bool) ~ Bool,
                       SqlEq expr (Columnar f FlagPublished), SqlValable (expr Bool),
                       SqlValable (Columnar f FlagPublished)) 
                       => Maybe (HaskellLiteralForQExpr (Columnar f FlagPublished))
                       -> NewsT f -> expr Bool
filterFlagPublished (Just fp) n = (_newsPublic n) ==. (val_ fp)
filterFlagPublished Nothing _ = val_ True
    
-- filterNewsName :: Maybe NewsName -> NewsT (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool
filterNewsName :: (HaskellLiteralForQExpr (expr Bool) ~ Bool,
                  SqlEq expr (Columnar f NameNews), SqlValable (expr Bool),
                  SqlValable (Columnar f NameNews)) 
                  => Maybe (HaskellLiteralForQExpr (Columnar f NameNews))
                  -> NewsT f -> expr Bool
filterNewsName (Just nn) n = (_newsName n) ==. (val_ nn)
filterNewsName Nothing _ = val_ True

-- filterCategory :: Maybe Category -> NewsT (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool
filterCategory :: (HaskellLiteralForQExpr (expr Bool) ~ Bool,
                  SqlEq expr (Columnar f Category), SqlValable (expr Bool),
                  SqlValable (Columnar f Category)) 
                  => Maybe (HaskellLiteralForQExpr (Columnar f Category))
                  -> NewsT f -> expr Bool
filterCategory (Just c) n = (_newsCategory n) ==. (val_ c)
filterCategory Nothing _ = val_ True

-- filterAuthor :: Maybe Name -> NewsT (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool
filterAuthor :: (HaskellLiteralForQExpr (expr Bool) ~ Bool,
                SqlEq expr (Columnar f Name), SqlValable (expr Bool),
                SqlValable (Columnar f Name)) 
                => Maybe (HaskellLiteralForQExpr (Columnar f Name))
                -> NewsT f -> expr Bool
filterAuthor (Just a) n = (_newsNameAuthor n) ==. (val_ a)
filterAuthor Nothing _ = val_ True

-- filterDaySince :: Maybe DaySince -> NewsT (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool
filterDaySince :: (HaskellLiteralForQExpr (expr Bool) ~ Bool,
                  SqlEq expr (Columnar f Day), SqlValable (expr Bool),
                  SqlValable (Columnar f Day)) 
                  => Maybe (HaskellLiteralForQExpr (Columnar f Day))
                  -> NewsT f -> expr Bool
filterDaySince (Just ds) n = (_newsDateCreation n) ==. (val_ ds) 
filterDaySince Nothing _ = val_ True

-- filterDayAt :: Maybe DayAt -> NewsT (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool
filterDayAt :: (HaskellLiteralForQExpr (expr Bool) ~ Bool,
               SqlOrd expr (Columnar f Day), SqlValable (expr Bool),
               SqlValable (Columnar f Day)) 
               => Maybe (HaskellLiteralForQExpr (Columnar f Day))
               -> NewsT f -> expr Bool
filterDayAt (Just dat) n = (_newsDateCreation n) >=. (val_ dat) 
filterDayAt Nothing _ = val_ True

-- filterDayUntil :: Maybe DayUntil -> NewsT (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool
filterDayUntil :: (HaskellLiteralForQExpr (expr Bool) ~ Bool,
                  SqlOrd expr (Columnar f Day), SqlValable (expr Bool),
                  SqlValable (Columnar f Day)) 
                  => Maybe (HaskellLiteralForQExpr (Columnar f Day))
                  -> NewsT f -> expr Bool
filterDayUntil (Just dat) n = (_newsDateCreation n) <=. (val_ dat) 
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
      let mn = fmap f $ newsTToNews x
      _ <- traverse (\n-> do
          BPC.runInsert c $ Beam.insert (_news newsDB) $ insertValues
            [ newsToNewsT n
            ]
        ) mn
      return ()
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
    { nameNews = _newsName n
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
  { _newsName = nameNews n
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
    { _newsName = nn
    , _newsLoginAuthor = undefined
    , _newsNameAuthor = undefined
    , _newsDateCreation = undefined
    , _newsCategory = undefined
    , _newsContent = undefined
    , _newsPhoto = undefined -- PhotoVector-- [Photo] -- (Vector Photo)
    , _newsPublic = undefined
    }

data NewsT f = NewsT
  { _newsName :: Columnar f NameNews
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
  primaryKey = NewsId . _newsName

data NewsDB f = NewsDB
  { _news :: f (TableEntity NewsT) }
  deriving (Generic, Database be)

newsDB :: DatabaseSettings be NewsDB
newsDB = defaultDbSettings
{-
data UserT f = UserT
  { _userName :: Columnar f Name
  , _userLogin :: Columnar f Login
  , _userPasswordHash :: Columnar f ByteString
  , _userDateCreation :: Columnar f Day
  , _userAdmin :: Columnar f FlagAdmin
  , _userMakeNews :: Columnar f FlagMakeNews
  } deriving (Generic, Beamable)

type UserTId = UserT Identity
type UserId = PrimaryKey UserT Identity

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Login) 
    deriving (Generic, Beamable)
  primaryKey = UserId . _userLogin

data AccountDB f = AccountDB
  { _accounts :: f (TableEntity UserT) } 
  deriving (Generic, Database be)

accountDB :: DatabaseSettings be AccountDB
accountDB = defaultDbSettings 
-}

