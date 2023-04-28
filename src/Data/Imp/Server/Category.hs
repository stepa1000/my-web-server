{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Imp.Server.Category
  ( withHandle,
  )
where

import qualified Control.Logger as Logger
import Control.Monad
import qualified Control.Server.Category as Category
import Data.Imp.Database
import Data.Maybe as Maybe
import Data.Tree as Tree
import Data.Types
import Data.Utils
import Data.Vector as V
import Database.Beam.Postgres
import Database.Beam.Postgres.Conduit as BPC
import Database.Beam.Query as Beam
import Database.Beam.Schema.Tables
import System.Random
import Prelude as P

-- | implementation for handling database calls related to the catechory table.
--
-- the handler structure contains functions that have been written before.
--
-- to pass the configured functions to the
-- message processing loop of the client to execute the above described functions.
withHandle :: Logger.Handle IO -> Connection -> (Category.Handle IO -> IO a) -> IO a
withHandle hl c g = do
  _ <- initNewsCategory hl c "General"
  g $
    Category.Handle
      { Category.hGetCategory = fromMaybe (Node "" []) <$> getNewsCategory hl c "General",
        Category.hChangeCategory = changeCategory hl c,
        Category.hCreateCategory = createCategory hl c
      }

-- | change the ancestor of a category or name.
--
-- The fields in the specified row of the table are
-- updated according to the above mentioned fields.
--
-- to improve the hierarchy of topic approximation.
changeCategory :: Logger.Handle IO -> Connection -> Category -> Maybe Category -> Maybe Category -> IO ()
changeCategory hl c cat cr cnn = do
  Logger.logInfo hl "Change category"
  mcatT <- getCategory hl c cat
  _ <-
    BPC.runUpdate c $
      Beam.updateTable
        (dbCategory webServerDB)
        ( CategoryT
            { _categoryUuidCategory = toOldValue,
              _categoryCategoryName =
                toUpdatedValueMaybe $
                  const $
                    fmap val_ cnn,
              _categoryParent =
                toUpdatedValueMaybe $
                  const $
                    fmap val_ cr,
              _categoryChild = toOldValue
            }
        )
        (\cat2 -> _categoryCategoryName cat2 ==. val_ cat)
  P.mapM_
    ( \(p, catT) ->
        do
          _ <-
            BPC.runUpdate c $
              Beam.updateTable
                (dbCategory webServerDB)
                ( CategoryT
                    { _categoryUuidCategory = toOldValue,
                      _categoryCategoryName = toOldValue,
                      _categoryParent = toOldValue,
                      _categoryChild = toUpdatedValue $
                        \v -> arrayRemove (_categoryChild v) (val_ cat)
                    }
                )
                (\cat2 -> _categoryCategoryName cat2 ==. val_ (_categoryParent catT))
          _ <-
            BPC.runUpdate c $
              Beam.updateTable
                (dbCategory webServerDB)
                ( CategoryT
                    { _categoryUuidCategory = toOldValue,
                      _categoryCategoryName = toOldValue,
                      _categoryParent = toOldValue,
                      _categoryChild = toUpdatedValue $
                        \v -> concatV_ (_categoryChild v) (val_ $ V.singleton cat)
                    }
                )
                (\cat2 -> _categoryCategoryName cat2 ==. val_ p)
          return ()
    )
    (cr >>= (\x -> (,) x <$> mcatT))

-- | Initializing the most common category or the first vertex of the tree.
--
-- The general category is queried and if nothing is returned, it is created.
--
-- for convenience. The vertex will always be checked before
-- configuring the handler and will always have the same name.
initNewsCategory :: Logger.Handle IO -> Connection -> Category -> IO ()
initNewsCategory hl c ca = do
  mc <- getNewsCategory hl c ca
  case mc of
    (Just _) -> return ()
    Nothing -> do
      ruuid <- randomIO
      _ <-
        BPC.runInsert c $
          Beam.insert
            (dbCategory webServerDB)
            ( Beam.insertValues
                [ CategoryT
                    { _categoryUuidCategory = ruuid,
                      _categoryCategoryName = ca,
                      _categoryParent = "",
                      _categoryChild = V.empty
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
createCategory hl c car can = do
  Logger.logInfo hl "Create category"
  _ <-
    BPC.runUpdate c $
      Beam.update
        (dbCategory webServerDB)
        (\cat -> _categoryChild cat <-. concatV_ (current_ (_categoryChild cat)) (val_ $ V.singleton can))
        (\cat -> _categoryCategoryName cat ==. val_ car)
  ruuid <- randomIO
  _ <-
    BPC.runInsert c $
      insert
        (dbCategory webServerDB)
        ( insertValues
            [ CategoryT
                { _categoryUuidCategory = ruuid,
                  _categoryCategoryName = can,
                  _categoryParent = car,
                  _categoryChild = V.empty
                }
            ]
        )
  return ()

-- | Taking the full tree of news categories.
--
-- The specified category and all subcategories specified as children are taken.
-- Then they are merged into the whole tree.
--
-- it is necessary for a holistic view of the tree structure and
-- easy viewing and sending to the server's clients.
getNewsCategory :: Logger.Handle IO -> Connection -> Category -> IO (Maybe NewsCategory)
getNewsCategory hl c ca = do
  mc <- getCategory hl c ca
  case mc of
    (Just catT) -> do
      vc <- V.catMaybes <$> P.mapM (getNewsCategory hl c) (_categoryChild catT)
      return $ Just $ Node ca (V.toList vc)
    Nothing -> return Nothing

getCategory :: Logger.Handle IO -> Connection -> Category -> IO (Maybe CategoryTId)
getCategory hl c ca = do
  Logger.logInfo hl "Get category parrent"
  l <- listStreamingRunSelect c $ lookup_ (dbCategory webServerDB) (primaryKey $ categoryName ca)
  return $ listToMaybe l
