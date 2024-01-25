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
    debugPosition,
    hSearchContent,
    filterContent',
    sortNews,
  )
where

import qualified Control.Logger as Logger
import qualified Control.Server.News as SNews
import Data.ByteString
import Data.Foldable
import Data.Imp.Database
import qualified Data.Imp.Server.Photo as ImpSPhoto
import Data.List (sortBy)
import Data.Maybe as Maybe
import Data.News
import Data.String
import Data.Text.Internal
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Types
import Data.UUID
import Data.Utils
import Data.Vector as V
import Data.Yaml as Y
import Database.Beam as Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Database.Beam.Query.Internal
import System.Random
import Prelude as P

-- | Config with a field that stores a value about the
-- maximum length of the list when the query is output.
newtype Config = Config
  {confMaxLimit :: Integer}
  deriving (Generic)
  deriving anyclass (Y.ToJSON, Y.FromJSON)

-- | Handler Assembler.
--
-- Sets the implanted handler functions to the appropriate fields.
--
-- To have a handler that is usable in practice.
makeHandle :: Logger.Handle IO -> Config -> Connection -> SNews.Handle IO
makeHandle logger config connectDB =
  SNews.Handle
    { SNews.handlePhoto = ImpSPhoto.makeHandle logger connectDB,
      SNews.hSearchNews = hSearchNews logger (confMaxLimit config) connectDB,
      SNews.hPutNews = hPutNews logger connectDB,
      SNews.hGenUUID = randomIO,
      SNews.hGetNews = hGetNews logger connectDB,
      SNews.hModifNews = hModifNews logger connectDB,
      SNews.hGetDay = hGetDay
    }

-- | Database query. Search for news by filtering and
-- outputting unpublished news from the user.
--
-- The application of the query depends on the
-- search options and whether a login is specified.
-- Also apply restrictions on the issuance of the number of news.
--
-- The query itself to the database was taken into a separate function
-- because of the number of options for which the search takes place,
-- so in the current function in addition to this remain commands
-- to access the database for the entire query.
hSearchNews :: Logger.Handle IO -> Integer -> Connection -> Maybe Login -> Search -> IO [News]
hSearchNews logger maxLimit connectDB mlogin search = do
  Logger.logInfo logger "Search news"
  lMNews <-
    (fmap . fmap) newsTToNews $
      listStreamingRunSelect connectDB $
        select $
          searchNews (toInteger <$> mLimit search) (toInteger <$> mOffSet search)
  return $ sortNews (mSortBy search) $ Maybe.catMaybes lMNews
  where
    searchNews (Just limit) (Just offset) =
      limit_ (min maxLimit limit) $
        offset_ offset $
          filter_ (filterSearch mlogin search) (all_ (dbNews webServerDB))
    searchNews _ _ = limit_ maxLimit $ offset_ 0 $ filter_ (filterSearch mlogin search) (all_ (dbNews webServerDB))

hSearchContent :: Integer -> Connection -> Content -> IO [News]
hSearchContent maxLimit connectDB content = do
  lMNews <- (fmap . fmap) newsTToNews $ listStreamingRunSelect connectDB $ select $ searchNews Nothing Nothing
  return $ Maybe.catMaybes lMNews
  where
    searchNews (Just limit) (Just offset) =
      limit_ (min maxLimit limit) $
        offset_ offset $
          filter_' (filterContent' (Just content)) (all_ (dbNews webServerDB))
    searchNews _ _ =
      limit_ maxLimit $ offset_ 0 $ filter_' (filterContent' (Just content)) (all_ (dbNews webServerDB))

debugPosition :: Connection -> Content -> IO ByteString
debugPosition connectDB content = do
  pgTraceStmtIO' @(SqlSelect Postgres Integer) connectDB $ select $ do
    news <- all_ (dbNews webServerDB)
    return $ position (val_ content) (_newsContent news)

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

-- | Search query for news in the database.
--
-- Each search option is converted into a query that checks
-- the equality of the specified data with the data from the news
-- in the database, then connect them using the operator "and".
-- There is a search string in different fields of the news.
-- It is also possible to publish unpublished news to the user.
--
-- To create a filter by conditions in one function.
filterSearch ::
  CxtFilterSearch f =>
  Maybe Login ->
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
filterSearch mlogin (Search mDayAt' mDayUntil' mDaySince' mAuthor' mCategory' mNewsUUID' mNewsNam' mContent' mForString' mFlagPublished' _ _ _) news =
  filterDaySince mDaySince' news
    &&. filterDayAt mDayAt' news
    &&. filterDayUntil mDayUntil' news
    &&. filterAuthor mAuthor' news
    &&. filterCategory mCategory' news
    &&. filterUUID mNewsUUID' news
    &&. filterNewsName mNewsNam' news
    &&. filterFlagPublished mlogin mFlagPublished' news
    &&. filterContent mContent' news
    &&. f mForString'
  where
    f (Just forString) =
      filterForStringName forString news
        ||. filterForStringContent forString news
        ||. filerForStringAuthor forString news
        ||. filterForStringCategory forString news
    f Nothing = val_ True

filterUUID ::
  ( HaskellLiteralForQExpr (expr Bool) ~ Bool,
    SqlEq expr (Columnar f UUID),
    SqlValable (expr Bool),
    SqlValable (Columnar f UUID)
  ) =>
  Maybe (HaskellLiteralForQExpr (Columnar f UUID)) ->
  NewsT f ->
  expr Bool
filterUUID (Just uuID) news = _newsUuidNews news ==. val_ uuID
filterUUID Nothing _ = val_ True

filterForStringContent ::
  (Columnar f Content ~ QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Content) =>
  Data.Text.Internal.Text ->
  NewsT f ->
  QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Bool
filterForStringContent forString news = positionQNested (val_ forString) (_newsContent news) /=. val_ 0

filterForStringName ::
  (Columnar f NameNews ~ QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Content) =>
  Text ->
  NewsT f ->
  QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Bool
filterForStringName forString news = positionQNested (val_ forString) (_newsNewsName news) /=. val_ 0

filerForStringAuthor ::
  (Columnar f Name ~ QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Content) =>
  Text ->
  NewsT f ->
  QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Bool
filerForStringAuthor forString news = positionQNested (val_ forString) (_newsNameAuthor news) /=. val_ 0

filterForStringCategory ::
  (Columnar f Category ~ QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Content) =>
  Text ->
  NewsT f ->
  QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Bool
filterForStringCategory forString news = positionQNested (val_ forString) (_newsCategory news) /=. val_ 0

filterContent ::
  (Columnar f Content ~ QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Content) =>
  Maybe Text ->
  NewsT f ->
  QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Bool
filterContent (Just content) news = positionQNested (val_ content) (_newsContent news) /=. val_ 0
filterContent Nothing _ = val_ True

filterContent' ::
  (Columnar f Content ~ QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) Content) =>
  Maybe Text ->
  NewsT f ->
  QGenExpr QValueContext Postgres (QNested (QNested QBaseScope)) SqlBool
filterContent' (Just content) news = positionQNested (val_ content) (_newsContent news) /=?. val_ 0
filterContent' Nothing _ = sqlBool_ $ val_ True

filterFlagPublished ::
  ( SqlEq (QGenExpr context Postgres s) (Columnar f FlagPublished),
    SqlEq (QGenExpr context Postgres s) (Columnar f Login),
    SqlValable (Columnar f FlagPublished),
    SqlValable (Columnar f Login)
  ) =>
  Maybe (HaskellLiteralForQExpr (Columnar f Login)) ->
  Maybe (HaskellLiteralForQExpr (Columnar f FlagPublished)) ->
  NewsT f ->
  QGenExpr context Postgres s Bool
filterFlagPublished (Just login) (Just flagPub) news =
  (_newsPublic news ==. val_ flagPub) &&. (_newsLoginAuthor news ==. val_ login)
filterFlagPublished _ _ _ = val_ True

filterNewsName ::
  ( HaskellLiteralForQExpr (expr Bool) ~ Bool,
    SqlEq expr (Columnar f NameNews),
    SqlValable (expr Bool),
    SqlValable (Columnar f NameNews)
  ) =>
  Maybe (HaskellLiteralForQExpr (Columnar f NameNews)) ->
  NewsT f ->
  expr Bool
filterNewsName (Just nameNews') news = _newsNewsName news ==. val_ nameNews'
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
filterCategory (Just category) news = _newsCategory news ==. val_ category
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
filterAuthor (Just name) news = _newsNameAuthor news ==. val_ name
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
filterDaySince (Just day) news = _newsDateCreation news ==. val_ day
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
filterDayAt (Just dat) news = _newsDateCreation news >=. val_ dat
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
filterDayUntil (Just dat) news = _newsDateCreation news <=. val_ dat
filterDayUntil Nothing _ = val_ True

hGetDay :: IO Day
hGetDay = do
  (UTCTime d _) <- getCurrentTime
  return d

hModifNews :: Logger.Handle IO -> Connection -> UUID -> (News -> News) -> IO ()
hModifNews logger connectDB uuID act = do
  Logger.logInfo logger "Modify news"
  lNewsT <- listStreamingRunSelect connectDB $ lookup_ (dbNews webServerDB) (primaryKey $ uuidNewsT uuID)
  case lNewsT of
    (newsT : _) -> do
      _ <-
        BPC.runDelete connectDB $
          delete
            (dbNews webServerDB)
            (\n -> _newsNewsName n ==. val_ (_newsNewsName newsT))
      let mNews = act <$> newsTToNews newsT
      traverse_
        ( \n -> do
            BPC.runInsert connectDB $
              Beam.insert (dbNews webServerDB) $
                insertValues
                  [ newsToNewsT n
                  ]
        )
        mNews
    [] -> return ()

hGetNews :: Logger.Handle IO -> Connection -> UUID -> IO (Maybe News)
hGetNews logger connectDB uuID = do
  Logger.logInfo logger "Gut news"
  lNewsT <- listStreamingRunSelect connectDB $ lookup_ (dbNews webServerDB) (primaryKey $ uuidNewsT uuID)
  case lNewsT of
    (newsT : _) -> return $ newsTToNews newsT
    [] -> return Nothing

hPutNews :: Logger.Handle IO -> Connection -> News -> IO ()
hPutNews logger connectDB news = do
  Logger.logInfo logger "Put news"
  _ <-
    BPC.runInsert connectDB $
      Beam.insert (dbNews webServerDB) $
        insertValues
          [ newsToNewsT news
          ]
  return ()

type CxtFilterSearch f =
  ( SqlValable (Columnar f UUID),
    SqlEq (QGenExpr QValueContext Postgres (QNested (QNested QBaseScope))) (Columnar f UUID),
    HaskellLiteralForQExpr (Columnar f UUID) ~ UUID,
    Columnar f NameNews
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
