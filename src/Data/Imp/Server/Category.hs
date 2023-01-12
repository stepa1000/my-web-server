{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Imp.Server.Category 
  ( withHandle
  , initNewsCategory'
  ) where

import Prelude as P

-- import GHC.Generics
{-
import Database.Beam as Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Conduit
-}
--import System.Random
--import Control.Applicative
--import Control.Monad.Catch
import Control.Monad

--import Data.Text
--import Data.ByteString
--import Data.Binary
--import Data.ByteArray

--import Data.Time.Calendar.OrdinalDate
--import Data.Time.Clock
--import Crypto.Hash
--import Crypto.Hash.IO

import Data.Maybe as Maybe
--import System.IO
-- import Data.Typeable
-- import Data.UUID
-- import Data.Vector as V

import Data.Aeson as A
import Data.List as List (unzip)
import Data.IORef
import Data.Tree as Tree

--import Data.News
--import Data.User
import Data.Types
--import Data.Utils

import qualified Control.Server.Category as Category

withHandle :: FilePath -> (Category.Handle IO -> IO a) -> IO a
withHandle fp g = do
  mnc <- decodeFileStrict fp
  case mnc of
    (Just nc) -> do
      rnc <- newIORef nc
      a <- g $ Category.Handle
        { Category.hGetCategory = hGetCategory rnc
        , Category.hChangeCategory = hChangeCategory rnc
        , Category.hCreateCategory = hCreateCategory rnc
        }
      nc2 <- readIORef rnc
      A.encodeFile fp nc2
      return a
    _ -> error "fail is not opening"

hGetCategory :: IORef NewsCategory -> IO NewsCategory
hGetCategory nc = readIORef nc

initNewsCategory' :: String -> IO ()
initNewsCategory' fp = do
  encodeFile fp $ Node "General" []

hChangeCategory :: IORef NewsCategory -> Category -> Maybe Category -> Maybe Category -> IO ()
hChangeCategory rnc c (Just nrc) (Just nnc) = do
  modifyIORef rnc (\nc -> cutAddTree nc nrc c nnc)
hChangeCategory rnc c (Just nrc) Nothing = do
  modifyIORef rnc (\nc -> cutAddTree nc nrc c c)
hChangeCategory rnc c Nothing (Just nnc) = do
  modifyIORef rnc (\nc -> renameNode nc c nnc)
hChangeCategory _ _ Nothing Nothing = return () -- loging the

hCreateCategory :: IORef NewsCategory -> Category -> Category -> IO ()
hCreateCategory rnc cr cn = do
  modifyIORef rnc (\t-> addTree t cr [Node cn []])
{-
catMaybesTree :: a -> Tree (Maybe a) -> Tree a
catMaybesTree t 
  | isJust (rootLable t) = Node (fromJust (rootLable t)) (fmap catMaybesTree (subForest t))
  | not $ P.null (fmap catMaybesTree (subForest t)) = 
    Node (head (fmap catMaybesTree (subForest t))) (tail (fmap catMaybesTree (subForest t)))
  | True = Node a _
-}
cutAddTree :: Eq a => Tree a -> a -> a -> a -> Tree a
cutAddTree t e n n2 = f $ cutTree t n
  where
    f (sf,mt) = maybe (Node n sf) id (fmap (\tn-> addTree tn e [Node n2 sf] ) mt)

addTree :: Eq a => Tree a -> a -> [Tree a] -> Tree a
addTree t r lsf = snd $ mapTree t r f
  where
    f a sf = ((),a,lsf ++ sf)

cutTree :: Eq a => Tree a -> a -> ([Tree a],Maybe (Tree a) )
cutTree t a = (\(x,y)-> (catMaybes $ fmap catMaybesTree x, catMaybesTree y) ) $ 
  cutTree' (fmap Just t) (Just a)

catMaybesTree :: Tree (Maybe a) -> Maybe (Tree a)
catMaybesTree = foldTree f
  where
    f :: Maybe a -> [Maybe (Tree a)] -> Maybe (Tree a)
    f ma t = ma >>= (\a-> return $ Node a (catMaybes t))

cutTree' :: Eq a => Tree (Maybe a) -> Maybe a -> ([Tree (Maybe a)],Tree (Maybe a))
cutTree' t a = (\(x,y)->(join x,y)) $ mapTree t a f 
  where
    f (Just _) sf = (sf,Nothing,[])
    f Nothing _ = ([],Nothing,[])

renameNode :: Eq a => Tree a -> a -> a -> Tree a
renameNode t a an = snd $ mapTree t a (\_ ft->((),an,ft))

mapTree :: Eq a => Tree a -> a -> (a -> [Tree a] -> (b,a,[Tree a])) -> ([b],Tree a)
mapTree t a f 
  | (rootLabel t) == a = (\(b,a2,sf)->([b],Node a2 sf) ) $ f a (subForest t)
  | not $ P.null (subForest t) = (\(llb,sf)->(join llb, Node (rootLabel t) sf) ) $ 
    List.unzip  $ fmap (\tn-> mapTree tn a f) (subForest t)
  | True = ([],t)
