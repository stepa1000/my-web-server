{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Imp.Server.News
  ( Config (..),
    makeHandle,
    NewsT (..),
    NewsDB (..),
    newsDB,
    hSearchContent,
    debugPosition,
  )
where

import qualified Control.Logger as Logger
import qualified Control.Server.News as SNews
import Data.Aeson as A
import Data.ByteString
import Data.Foldable
import qualified Data.Imp.Server.Photo as ImpSPhoto
import Data.List (sortBy)
import Data.Maybe as Maybe
import Data.News
import Data.String
import Data.Text.Internal
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Types
import Data.Utils
import Data.Vector as V
import Data.Yaml as Y
import Database.Beam as Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Database.Beam.Query.Internal
import Prelude as P

newtype Config = Config
  {confMaxLimit :: Integer}
  deriving (Generic)
  deriving anyclass (Y.ToJSON, Y.FromJSON)

makeHandle :: Logger.Handle IO -> Config -> Connection -> SNews.Handle IO
makeHandle hl conf c =
  SNews.Handle
    { SNews.handlePhoto = ImpSPhoto.makeHandle hl c,
      SNews.hSearchNews = hSearchNews hl (confMaxLimit conf) c,
      SNews.hPutNews = hPutNews hl c,
      SNews.hGetNews = hGetNews hl c,
      SNews.hModifNews = hModifNews hl c,
      SNews.hGetDay = hGetDay
    }

hSearchNews :: Logger.Handle IO -> Integer -> Connection -> Search -> IO [News]
hSearchNews hl maxLimit c s = do
  Logger.logInfo hl "Search news"
  lm <- (fmap . fmap) newsTToNews $ listStreamingRunSelect c $ select $ searchNews (toInteger <$> mLimit s) (toInteger <$> mOffSet s)
  return $ sortNews (mSortBy s) $ Maybe.catMaybes lm
  where
    searchNews (Just l) (Just o) = limit_ (min maxLimit l) $ offset_ o $ filter_ (filterSearch s) (all_ (_news newsDB))
    searchNews _ _ = limit_ maxLimit $ offset_ 0 $ filter_ (filterSearch s) (all_ (_news newsDB))

hSearchContent :: Integer -> Connection -> Content -> IO [News]
hSearchContent maxLimit c content = do
  lm <- (fmap . fmap) newsTToNews $ listStreamingRunSelect c $ select $ searchNews Nothing Nothing
  return $ Maybe.catMaybes lm
  where
    searchNews (Just l) (Just o) = limit_ (min maxLimit l) $ offset_ o $ filter_' (filterContent' (Just content)) (all_ (_news newsDB))
    searchNews _ _ = limit_ maxLimit $ offset_ 0 $ filter_' (filterContent' (Just content)) (all_ (_news newsDB))

debugPosition :: Connection -> Content -> IO ByteString
debugPosition c content = do
  pgTraceStmtIO' @(SqlSelect Postgres Integer) c $ select $ do
    n <- all_ (_news newsDB)
    return $ position (val_ content) (_newsContent n)

position :: QGenExpr QValueContext Postgres QBaseScope Content -> QGenExpr QValueContext Postgres QBaseScope Content -> QGenExpr QValueContext Postgres QBaseScope Integer
position = customExpr_ f
  where
    f :: (Monoid a, IsString a) => a -> a -> a
    f c1 c2 = "position(" <> c1 <> " IN " <> c2 <> ")"

positionQNested ::
  QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Content ->
  QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Content ->
  QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Integer
positionQNested = customExpr_ f
  where
    f :: (Monoid a, IsString a) => a -> a -> a
    f c1 c2 = "position(" <> c1 <> " IN " <> c2 <> ")"

sortNews :: Maybe SortBy -> [News] -> [News]
sortNews (Just SBDate) = sortBy (\a b -> compare (dateCreationNews a) (dateCreationNews b))
sortNews (Just SBAuthor) = sortBy (\a b -> compare (nameAuthor a) (nameAuthor b))
sortNews (Just SBCategory) = sortBy (\a b -> compare (categoryNews a) (categoryNews b))
sortNews (Just SBCountPhoto) = sortBy (\a b -> compare (V.length $ photoNews a) (V.length $ photoNews b))
sortNews Nothing = id

--
filterSearch ::
  CxtFilterSearch f =>
  Search ->
  NewsT f ->
  QGenExpr
    QValueContext
    Postgres
    ( QNested
        ( QNested
            QBaseScope
        )
    )
    Bool
filterSearch (Search mDayAt' mDayUntil' mDaySince' mAuthor' mCategory' mNewsNam' mContent' mForString' mFlagPublished' _ _ _) n =
  filterDaySince mDaySince' n
    &&. filterDayAt mDayAt' n
    &&. filterDayUntil mDayUntil' n
    &&. filterAuthor mAuthor' n
    &&. filterCategory mCategory' n
    &&. filterNewsName mNewsNam' n
    &&. filterFlagPublished mFlagPublished' n
    &&. filterContent mContent' n
    &&. f mForString'
  where
    f (Just fs) =
      filterForStringName fs n
        ||. filterForStringContent fs n
        ||. filerForStringAuthor fs n
        ||. filterForStringCategory fs n
    f Nothing = val_ True

filterForStringContent ::
  (Columnar f Content ~ QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Content) =>
  Data.Text.Internal.Text ->
  NewsT f ->
  QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Bool
filterForStringContent fs n = positionQNested (val_ fs) (_newsContent n) /=. val_ 0

filterForStringName ::
  (Columnar f NameNews ~ QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Content) =>
  Text ->
  NewsT f ->
  QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Bool
filterForStringName s n = positionQNested (val_ s) (_newsNewsName n) /=. val_ 0

filerForStringAuthor ::
  (Columnar f Name ~ QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Content) =>
  Text ->
  NewsT f ->
  QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Bool
filerForStringAuthor s n = positionQNested (val_ s) (_newsNameAuthor n) /=. val_ 0

filterForStringCategory ::
  (Columnar f Category ~ QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Content) =>
  Text ->
  NewsT f ->
  QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Bool
filterForStringCategory s n = positionQNested (val_ s) (_newsCategory n) /=. val_ 0

filterContent ::
  (Columnar f Content ~ QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Content) =>
  Maybe Text ->
  NewsT f ->
  QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Bool
filterContent (Just c) n = positionQNested (val_ c) (_newsContent n) /=. val_ 0
filterContent Nothing _ = val_ True

filterContent' ::
  (Columnar f Content ~ QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Content) =>
  Maybe Text ->
  NewsT f ->
  QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) SqlBool
filterContent' (Just c) n = positionQNested (val_ c) (_newsContent n) /=?. val_ 0
filterContent' Nothing _ = sqlBool_ $ val_ True

filterFlagPublished ::
  ( HaskellLiteralForQExpr (expr Bool) ~ Bool,
    SqlEq expr (Columnar f FlagPublished),
    SqlValable (expr Bool),
    SqlValable (Columnar f FlagPublished)
  ) =>
  Maybe (HaskellLiteralForQExpr (Columnar f FlagPublished)) ->
  NewsT f ->
  expr Bool
filterFlagPublished (Just fp) n = _newsPublic n ==. val_ fp
filterFlagPublished Nothing _ = val_ True

filterNewsName ::
  ( HaskellLiteralForQExpr (expr Bool) ~ Bool,
    SqlEq expr (Columnar f NameNews),
    SqlValable (expr Bool),
    SqlValable (Columnar f NameNews)
  ) =>
  Maybe (HaskellLiteralForQExpr (Columnar f NameNews)) ->
  NewsT f ->
  expr Bool
filterNewsName (Just nn) n = _newsNewsName n ==. val_ nn
filterNewsName Nothing _ = val_ True

filterCategory ::
  ( HaskellLiteralForQExpr (expr Bool) ~ Bool,
    SqlEq expr (Columnar f Category),
    SqlValable (expr Bool),
    SqlValable (Columnar f Category)
  ) =>
  Maybe (HaskellLiteralForQExpr (Columnar f Category)) ->
  NewsT f ->
  expr Bool
filterCategory (Just c) n = _newsCategory n ==. val_ c
filterCategory Nothing _ = val_ True

filterAuthor ::
  ( HaskellLiteralForQExpr (expr Bool) ~ Bool,
    SqlEq expr (Columnar f Name),
    SqlValable (expr Bool),
    SqlValable (Columnar f Name)
  ) =>
  Maybe (HaskellLiteralForQExpr (Columnar f Name)) ->
  NewsT f ->
  expr Bool
filterAuthor (Just a) n = _newsNameAuthor n ==. val_ a
filterAuthor Nothing _ = val_ True

filterDaySince ::
  ( HaskellLiteralForQExpr (expr Bool) ~ Bool,
    SqlEq expr (Columnar f Day),
    SqlValable (expr Bool),
    SqlValable (Columnar f Day)
  ) =>
  Maybe (HaskellLiteralForQExpr (Columnar f Day)) ->
  NewsT f ->
  expr Bool
filterDaySince (Just ds) n = _newsDateCreation n ==. val_ ds
filterDaySince Nothing _ = val_ True

filterDayAt ::
  ( HaskellLiteralForQExpr (expr Bool) ~ Bool,
    SqlOrd expr (Columnar f Day),
    SqlValable (expr Bool),
    SqlValable (Columnar f Day)
  ) =>
  Maybe (HaskellLiteralForQExpr (Columnar f Day)) ->
  NewsT f ->
  expr Bool
filterDayAt (Just dat) n = _newsDateCreation n >=. val_ dat
filterDayAt Nothing _ = val_ True

filterDayUntil ::
  ( HaskellLiteralForQExpr (expr Bool) ~ Bool,
    SqlOrd expr (Columnar f Day),
    SqlValable (expr Bool),
    SqlValable (Columnar f Day)
  ) =>
  Maybe (HaskellLiteralForQExpr (Columnar f Day)) ->
  NewsT f ->
  expr Bool
filterDayUntil (Just dat) n = _newsDateCreation n <=. val_ dat
filterDayUntil Nothing _ = val_ True

hGetDay :: IO Day
hGetDay = do
  (UTCTime d _) <- getCurrentTime
  return d

hModifNews :: Logger.Handle IO -> Connection -> NameNews -> (News -> News) -> IO ()
hModifNews hl c nn f = do
  Logger.logInfo hl "Modify news"
  l <- listStreamingRunSelect c $ lookup_ (_news newsDB) (primaryKey $ nameNewsT nn)
  case l of
    (x : _) -> do
      _ <-
        BPC.runDelete c $
          delete
            (_news newsDB)
            (\n -> _newsNewsName n ==. val_ (_newsNewsName x))
      let mn = f <$> newsTToNews x
      traverse_
        ( \n -> do
            BPC.runInsert c $
              Beam.insert (_news newsDB) $
                insertValues
                  [ newsToNewsT n
                  ]
        )
        mn
    [] -> return ()

hGetNews :: Logger.Handle IO -> Connection -> NewsName -> IO (Maybe News)
hGetNews hl c nn = do
  Logger.logInfo hl "Gut news"
  l <- listStreamingRunSelect c $ lookup_ (_news newsDB) (primaryKey $ nameNewsT nn)
  case l of
    (x : _) -> return $ newsTToNews x
    [] -> return Nothing

hPutNews :: Logger.Handle IO -> Connection -> News -> IO ()
hPutNews hl c n = do
  Logger.logInfo hl "Put news"
  _ <-
    BPC.runInsert c $
      Beam.insert (_news newsDB) $
        insertValues
          [ newsToNewsT n
          ]
  return ()

-- NewsT for beam

newsTToNews :: NewsTId -> Maybe News
newsTToNews n = do
  v <- A.decode $ fromStrict $ _newsPhoto n
  return $
    News
      { nameNews = _newsNewsName n,
        loginAuthor = _newsLoginAuthor n,
        nameAuthor = _newsNameAuthor n,
        dateCreationNews = _newsDateCreation n,
        categoryNews = _newsCategory n,
        textNews = _newsContent n,
        photoNews = v,
        publicNews = _newsPublic n
      }

newsToNewsT :: News -> NewsTId
newsToNewsT n =
  NewsT
    { _newsNewsName = nameNews n,
      _newsLoginAuthor = loginAuthor n,
      _newsNameAuthor = nameAuthor n,
      _newsDateCreation = dateCreationNews n,
      _newsCategory = categoryNews n,
      _newsContent = textNews n,
      _newsPhoto = toStrict $ A.encode $ photoNews n,
      _newsPublic = publicNews n
    }

nameNewsT :: NameNews -> NewsTId
nameNewsT nn =
  NewsT
    { _newsNewsName = nn,
      _newsLoginAuthor = undefined,
      _newsNameAuthor = undefined,
      _newsDateCreation = undefined,
      _newsCategory = undefined,
      _newsContent = undefined,
      _newsPhoto = undefined,
      _newsPublic = undefined
    }

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

-- type NewsId = PrimaryKey NewsT Identity

instance Table NewsT where
  data PrimaryKey NewsT f = NewsId (Columnar f NameNews)
    deriving (Generic, Beamable)
  primaryKey = NewsId . _newsNewsName

newtype NewsDB f = NewsDB
  {_news :: f (TableEntity NewsT)}
  deriving (Generic)
  deriving anyclass (Database be)

newsDB :: DatabaseSettings be NewsDB
newsDB = defaultDbSettings

-- Cxt

type CxtFilterSearch f =
  ( Columnar f NameNews
      ~ QGenExpr
          QValueContext
          Postgres
          ( QNested
              (QNested QBaseScope)
          )
          Content,
    HaskellLiteralForQExpr (Columnar f Day) ~ Day,
    HaskellLiteralForQExpr (Columnar f FlagPublished) ~ Bool,
    SqlOrd
      ( QGenExpr
          QValueContext
          Postgres
          (QNested (QNested QBaseScope))
      )
      (Columnar f Day),
    SqlEq
      (QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)))
      ( Columnar
          f
          Day
      ),
    SqlEq
      ( QGenExpr
          QValueContext
          Postgres
          ( QNested
              ( QNested
                  QBaseScope
              )
          )
      )
      (Columnar f FlagPublished),
    SqlValable (Columnar f Day),
    SqlValable (Columnar f FlagPublished)
  )
