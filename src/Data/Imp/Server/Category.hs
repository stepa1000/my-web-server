{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Imp.Server.Category
  ( withHandle,
  )
where

import Control.Exception
import qualified Control.Logger as Logger
import Control.Monad
import qualified Control.Server.Category as Category
import Data.Imp.Database
import Data.Maybe as Maybe
import Data.Text
import Data.Tree as Tree
import Data.Types
import Data.Utils
import Database.Beam.Postgres
import Database.Beam.Postgres.Conduit as BPC
import Database.Beam.Query as Beam
import Database.Beam.Schema.Tables
import Servant.Server
import System.Random
import Prelude as P

-- | implementation for handling database calls related to the catechory table.
--
-- the handler structure contains functions that have been written before.
--
-- to pass the configured functions to the
-- message processing loop of the client to execute the above described functions.
withHandle :: Logger.Handle IO -> Connection -> (Category.Handle IO -> IO a) -> IO a
withHandle logger connectDB act = do
  _ <- initNewsCategory logger connectDB "General"
  act $
    Category.Handle
      { Category.hGetCategory = fromMaybe (Node "" []) <$> getNewsCategory logger connectDB "General",
        Category.hChangeCategory = changeCategory logger connectDB,
        Category.hCreateCategory = createCategory logger connectDB,
        Category.hCreateCategoryWarn = createCategoryWarn logger
      }

-- | change the ancestor of a category or name.
--
-- The fields in the specified row of the table are
-- updated according to the above mentioned fields.
--
-- to improve the hierarchy of topic approximation.
changeCategory ::
  Logger.Handle IO ->
  Connection ->
  -- | Old category
  Category ->
  Maybe Category ->
  Maybe Category ->
  IO ()
changeCategory logger connectDB categoryOld mCategoryParent mCategoryName = do
  Logger.logInfo logger "Change category"
  void $ BPC.runUpdate connectDB $
      Beam.updateTable
        (dbCategory webServerDB)
        ( CategoryT
            { _categoryUuidCategory = toOldValue,
              _categoryCategoryName =
                toUpdatedValueMaybe $
                  const $
                    fmap val_ mCategoryName,
              _categoryParent =
                toUpdatedValueMaybe $
                  const $
                    fmap val_ mCategoryParent
            }
        )
        (\cat2 -> _categoryCategoryName cat2 ==. val_ categoryOld)

-- | Initializing the most common category or the first vertex of the tree.
--
-- The general category is queried and if nothing is returned, it is created.
--
-- for convenience. The vertex will always be checked before
-- configuring the handler and will always have the same name.
initNewsCategory :: Logger.Handle IO -> Connection -> Category -> IO ()
initNewsCategory logger connectDB category = do
  maybeCategory <- getNewsCategory logger connectDB category
  case maybeCategory of
    (Just _) -> return ()
    Nothing -> do
      ruuid <- randomIO
      _ <-
        BPC.runInsert connectDB $
          Beam.insert
            (dbCategory webServerDB)
            ( Beam.insertValues
                [ CategoryT
                    { _categoryUuidCategory = ruuid,
                      _categoryCategoryName = category,
                      _categoryParent = ""
                    }
                ]
            )
      return ()

-- | Creating category.
--
-- Writes the child category to the parent category and adds it to the database.
--
-- To create an hierarchy of configurations.
createCategory :: Logger.Handle IO -> Connection -> Category -> Category -> IO ()
createCategory logger connectDB categoryRoot vCategoryName = do
  Logger.logInfo logger "Create category"
  ruuid <- randomIO
  mCategoryRoot <- getCategory logger connectDB categoryRoot
  case (guard $ categoryRoot /= "") >> mCategoryRoot of
    (Just _) -> do
      eUnit <-
        try
          ( void $
              BPC.runInsert connectDB $
                insert
                  (dbCategory webServerDB)
                  ( insertValues
                      [ CategoryT
                          { _categoryUuidCategory = ruuid,
                            _categoryCategoryName = vCategoryName,
                            _categoryParent = categoryRoot
                          }
                      ]
                  )
          )
      case eUnit of
        (Right _) -> return ()
        (Left sqlE) -> handler sqlE >> throwIO (err401 {errBody = "Category exist."})
    _ -> do
      Logger.logWarning logger $ "Category root not exist: " Logger..< categoryRoot
      throwIO (err401 {errBody = "Category root not exist."})
  where
    handler :: SomeException -> IO ()
    handler exc = do
      Logger.logWarning logger $ "Unique violation for: " Logger..< vCategoryName
      Logger.logError logger $ "Exception: " <> (Data.Text.pack $ show exc)
      return ()

-- | Taking the full tree of news categories.
--
-- The specified category and all subcategories specified as children are taken.
-- Then they are merged into the whole tree.
--
-- it is necessary for a holistic view of the tree structure and
-- easy viewing and sending to the server's clients.
getNewsCategory :: Logger.Handle IO -> Connection -> Category -> IO (Maybe NewsCategory)
getNewsCategory logger connectDB category = do
  maybeCategory <- getCategory logger connectDB category
  case maybeCategory of
    (Just categoryT) -> do
      lNameCategory <- listStreamingRunSelect connectDB $ select $ do
        s <- Beam.all_ (dbCategory webServerDB) 
        guard_ $ (val_ $ _categoryCategoryName categoryT) ==. (_categoryParent s)
        pure $ _categoryCategoryName s
      lCategory <- Maybe.catMaybes <$> P.mapM (getNewsCategory logger connectDB) lNameCategory
      return $ Just $ Node category lCategory
    Nothing -> return Nothing

getCategory :: Logger.Handle IO -> Connection -> Category -> IO (Maybe CategoryTId)
getCategory logger connectDB category = do
  Logger.logInfo logger "Get category parrent"
  l <- listStreamingRunSelect connectDB $ lookup_ (dbCategory webServerDB) (primaryKey $ categoryName category)
  return $ listToMaybe l

createCategoryWarn :: Logger.Handle IO -> Category -> IO ()
createCategoryWarn logger category = do
  Logger.logInfo logger $ "Category is exist: " Logger..< category
  throwIO (err401 {errBody = "Category exist."})
