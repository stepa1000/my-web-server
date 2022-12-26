{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Imp.Server.Category where

import Prelude as P

import GHC.Generics

import Database.Beam as Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Conduit

import System.Random
import Control.Applicative
import Control.Monad.Catch
import Control.Monad

import Data.Text
import Data.ByteString
import Data.Binary
import Data.ByteArray

import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Crypto.Hash
import Crypto.Hash.IO

import Data.Maybe as Maybe
import Data.Typeable
import Data.UUID
import Data.Vector as V

import Data.Aeson as A
import Data.List as List
import Data.IORef
import Data.Tree as Tree

import Data.News
import Data.User
import Data.Types
import Data.Utils

import qualified Control.Server.Category as Category

hGetCategory :: IORef NewsCategory -> IO NewsCategory
hGetCategory nc = readIORef nc

hChengeCategory :: IORef NewsCategory -> Category -> Maybe Category -> Maybe Category -> IO ()
hChengeCategory rnc c (Just nrc) (Just nnc) = do
  nc <- readIORef rnc
  return ()
{-
catMaybesTree :: a -> Tree (Maybe a) -> Tree a
catMaybesTree t 
  | isJust (rootLable t) = Node (fromJust (rootLable t)) (fmap catMaybesTree (subForest t))
  | not $ P.null (fmap catMaybesTree (subForest t)) = 
    Node (head (fmap catMaybesTree (subForest t))) (tail (fmap catMaybesTree (subForest t)))
  | True = Node a _
-}
cutTree :: Tree a -> a -> ([Tree a],Maybe (Tree a) )
cutTree t a = cutTree'

catMaybesTree :: Tree (Maybe a) -> Maybe (Tree a)
catMaybesTree = foldTree f
  where
    f ma t = ma >>= (\a-> Node a (catMaybes t))

cutTree' :: Eq a => Tree (Maybe a) -> Maybe a -> ([Tree (Maybe a)],Tree (Maybe a))
cutTree' t a = mapTree t a $ f 
  where
    f (Just an) sf  (sf,Nothing,[])
    f Nothing _ = ([],Nothing,[])

mapTree :: Eq a => Tree a -> a -> (a -> [Tree a] -> (b,a,[Tree a])) -> ([b],Tree a)
mapTree t a f 
  | (rootLabel t) == a = (\(b,a2,sf)->([b],Node a2 sf) ) $ f a (subForest t)
  | not $ P.null (subForest t) = (\(llb,sf)->(join llb, Node (rootLabel t) sf) ) $ 
    List.unzip  $ fmap (\tn-> mapTree tn a f) (subForest t)
  | True = ([],t)
