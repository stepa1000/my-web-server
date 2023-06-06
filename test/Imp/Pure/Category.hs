module Imp.Pure.Category where

import qualified Control.Logger as Logger
import Control.Monad
import Control.Monad.State.Lazy
import qualified Control.Server.Category as Category
import Data.Imp.Database
import Data.Maybe as Maybe
import Data.Maybes
import Data.Tree
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

pureCategory :: Handle (State NewsCategory)
pureCategory =
  Handle
    { hGetCategory = pureGetCategory,
      hChangeCategory = pureChangeCategory,
      hCreateCategory = pureCreateCategory
    }

pureGetCategory :: State NewsCategory NewsCategory
pureGetCategory = get

pureChangeCategory :: Category -> Maybe Category -> Maybe Category -> State NewsCategory ()
pureChangeCategory name maybeRootNew maybeNewName =
  modify $ \newsCategory ->
    let foundCategory = (\(Tree n f) -> Tree (maybe n id maybeNewName) f) $ getCategory newsCategory
     in maybe newsCategory id $ do
          root <- maybeRootNew
          return $ setCategory root foundCategory newsCategory

getCategory (Tree _ []) = Nothing
getCategory (Tree node forest)
  | node == name = Just $ Tree node forest
  | True = sequence $ Tree (Just node) $ listToMaybe $ catMaybes $ fmap getCategory forest

setCategory root tree (Tree node forest)
  | root == name = Tree node (tree : forest)
  | True = Tree node $ fmap (setCategory root tree) forest

pureCreateCategory :: Category -> Category -> State NewsCategory ()
pureCreateCategory root name = modify $ setCategory root (Tree name [])
