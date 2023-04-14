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
import Data.Aeson as A
import Data.Bifunctor
import Data.IORef
import Data.Imp.Database
import Data.List as List (unzip)
import Data.Maybe as Maybe
import Data.Text
import Data.Tree as Tree
import Data.Types
import Data.Vector as V
import Database.Beam.Postgres
import Database.Beam.Postgres.Conduit as BPC
import Prelude as P

-- | implementation for handling database calls related to the catechory table.
--
-- the handler structure contains functions that have been written before.
--
-- to pass the configured functions to the
-- message processing loop of the client to execute the above described functions.
withHandle :: Logger.Handle IO -> Connection -> (Category.Handle IO -> IO a) -> IO a
withHandle hl c g = do
  g $
    Category.Handle
      { Category.hGetCategory = fromJust $ getNewsCategory hl c "General",
        Category.hChangeCategory = hChangeCategory hl,
        Category.hCreateCategory = hCreateCategory hl
      }

-- | Initializing the most common category or the first vertex of the tree.
--
-- The general category is queried and if nothing is returned, it is created.
--
-- for convenience. The vertex will always be checked before
-- configuring the handler and will always have the same name.
initNewsCategoty :: Logger.Handle IO -> Connection -> Category -> IO ()

initNewsCategory hl c ca = do
  mc <- getNewsCategory hl c ca
  case mc of
    (Just cat) -> return ()
    Nothing -> do

-- | Creating category.
--
-- Writes the child category to the parent category and adds it to the database.
--
-- To create an hierarchy of configurations.
createCategory :: Logger.Handle IO -> Connection -> Category -> Category -> IO ()
createCategory hl c car can = do
  runUpdate c $
    update
      (dbCategory webServerDB)
      (\cat -> _categoryChild cat <-. current_ (_categoryChild cat) V.++ (V.singletone can))
      (\cat -> _categoryCategoryName cat ==. val_ car)
  BPC.runInsert c $
    insert
      (dbCategory webServerDB)
      ( insertValues
          [ CategoryT
              { _categoryCategoryName = can,
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
      vc <- P.mapM (getNewsCategory hl c) (_categoryChild catT)
      return $ Node ca (V.tiList vc)

getCategory :: Logger.Handle IO -> Connection -> Category -> IO (Maybe CategoryTId)
getCategory hl c ca = do
  Logger.logInfo hl "Get category parrent"
  l <- listStreamingRunSelect c $ lookup_ (dbCategory webServerDB) (primaryKey $ categoryName ca)
  return $ listToMaybe l

hGetCategory :: Logger.Handle IO -> IORef NewsCategory -> IO NewsCategory
hGetCategory hl r = do
  Logger.logInfo hl "Get category"
  readIORef r

-- | changing category name and ancestor
--                                                              cut              addTo            rename
hChangeCategory :: Logger.Handle IO -> IORef NewsCategory -> Category -> Maybe Category -> Maybe Category -> IO ()
hChangeCategory hl rnc c (Just nrc) (Just nnc) = do
  Logger.logInfo hl "Change category: name and ancestor"
  modifyIORef rnc (\nc -> cutAddTree nc nrc c nnc)
hChangeCategory hl rnc c (Just nrc) Nothing = do
  Logger.logInfo hl "Change category: ancestor"
  modifyIORef rnc (\nc -> cutAddTree nc nrc c c)
hChangeCategory hl rnc c Nothing (Just nnc) = do
  Logger.logInfo hl "Change category: name"
  modifyIORef rnc (\nc -> renameNode nc c nnc)
hChangeCategory hl _ _ Nothing Nothing = do
  Logger.logInfo hl "Change category: Nothing"
  return ()

-- | adds categories under category
hCreateCategory :: Logger.Handle IO -> IORef NewsCategory -> Category -> Category -> IO ()
hCreateCategory hl rnc cr cn = do
  Logger.logInfo hl "Create category"
  modifyIORef rnc (\t -> addTree t cr [Node cn []])

--                            addTo cut rename
cutAddTree :: Eq a => Tree a -> a -> a -> a -> Tree a
cutAddTree t e n n2 = f $ cutTree t n
  where
    f (sf, mt) = maybe (Node n sf) (\tn -> addTree tn e [Node n2 sf]) mt

-- | adds subtrees
addTree :: Eq a => Tree a -> a -> [Tree a] -> Tree a
addTree t r lsf = snd $ mapTree t r f
  where
    f a sf = ((), a, lsf ++ sf)

-- | returns the subtrees and the remaining tree
cutTree :: Eq a => Tree a -> a -> ([Tree a], Maybe (Tree a))
cutTree t a =
  bimap (mapMaybe catMaybesTree) catMaybesTree $
    cutTree' (fmap Just t) (Just a)

catMaybesTree :: Tree (Maybe a) -> Maybe (Tree a)
catMaybesTree = foldTree f
  where
    f :: Maybe a -> [Maybe (Tree a)] -> Maybe (Tree a)
    f ma t = ma >>= (\a -> return $ Node a (catMaybes t))

cutTree' :: Eq a => Tree (Maybe a) -> Maybe a -> ([Tree (Maybe a)], Tree (Maybe a))
cutTree' t a = first join $ mapTree t a f
  where
    f (Just _) sf = (sf, Nothing, [])
    f Nothing _ = ([], Nothing, [])

renameNode :: Eq a => Tree a -> a -> a -> Tree a
renameNode t a an = snd $ mapTree t a (\_ ft -> ((), an, ft))

-- | take the tree and the name of the node
-- and the function the changes the node,
-- and returns the tree with the changed the node
mapTree :: Eq a => Tree a -> a -> (a -> [Tree a] -> (b, a, [Tree a])) -> ([b], Tree a)
mapTree t a f
  | rootLabel t == a = (\(b, a2, sf) -> ([b], Node a2 sf)) $ f a (subForest t)
  | not $ P.null (subForest t) =
      bimap join (Node (rootLabel t)) $
        List.unzip $
          fmap (\tn -> mapTree tn a f) (subForest t)
  | otherwise = ([], t)
