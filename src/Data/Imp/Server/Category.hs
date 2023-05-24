{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Imp.Server.Category
  ( withHandle,
    initNewsCategory',
  )
where

import qualified Control.Logger as Logger
import Control.Monad
import qualified Control.Server.Category as Category
import Data.Aeson as A
import Data.Bifunctor
import Data.IORef
import Data.List as List (unzip)
import Data.Maybe as Maybe
import Data.Text
import Data.Tree as Tree
import Data.Types
import Prelude as P

withHandle :: Logger.Handle IO -> FilePath -> (Category.Handle IO -> IO a) -> IO a
withHandle hl fp g = do
  mnc <- decodeFileStrict fp
  case mnc of
    (Just nc) -> do
      rnc <- newIORef nc
      a <-
        g $
          Category.Handle
            { Category.hGetCategory = hGetCategory hl rnc,
              Category.hChangeCategory = hChangeCategory hl rnc,
              Category.hCreateCategory = hCreateCategory hl rnc
            }
      nc2 <- readIORef rnc
      A.encodeFile fp nc2
      return a
    _ -> error "fail is not opening"

hGetCategory :: Logger.Handle IO -> IORef NewsCategory -> IO NewsCategory
hGetCategory hl r = do
  Logger.logInfo hl "Get category"
  readIORef r

initNewsCategory' :: String -> IO ()
initNewsCategory' fp = do
  encodeFile @(Tree Text) fp $ Node "General" []

-- | changing category name and ancestor
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
