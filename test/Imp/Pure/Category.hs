module Imp.Pure.Category (pureCategory, getCategory) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy as ST
import Control.Server.Category
import Data.Bifunctor
import Data.Bool
import Data.List (head, unzip)
import Data.Maybe as Maybe
import Data.Tree as Tree
import Data.Types
import Prelude (id, ($), (.), (==))
import qualified Prelude as P

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
    let (foundCategory, without) = first (fmap (\(root, Node node forest) -> (root, Node (maybe node id maybeNewName) forest))) $ getCategory name newsCategory
     in maybe newsCategory id $
          ( do
              (_, Node _ forest) <- foundCategory
              root <- maybeRootNew
              newName <- maybeNewName
              oldtree <- without
              return $ setCategory root (Node newName forest) oldtree
          )
            <|> ( do
                    (_, category) <- foundCategory
                    root <- maybeRootNew
                    oldtree <- without
                    return $ setCategory root category oldtree
                )
            <|> ( do
                    (root, Node _ forest) <- foundCategory
                    newName <- maybeNewName
                    oldtree <- without
                    return $ setCategory root (Node newName forest) oldtree
                )

getCategory :: Category -> Tree Category -> (Maybe (Category, Tree Category), Maybe (Tree Category))
getCategory name (Node node forest) = getCategory' node name (Node node forest)

getCategory' :: Category -> Category -> Tree Category -> (Maybe (Category, Tree Category), Maybe (Tree Category))
getCategory' root name (Node node forest)
  | node == name = (Just (root, Node node forest), Nothing)
  | not $ P.null forest =
      ( \((rootNow, trees), retree) ->
          ( do
              guard $ not $ P.null rootNow
              return (head rootNow, head trees),
            Just $ Node node retree
          )
      )
        $ bimap (unzip . join) join
        $ unzip
        $ fmap (bimap maybeToList maybeToList . getCategory' node name) forest
  | True = (Nothing, Just $ Node node [])

setCategory :: P.Eq t => t -> Tree t -> Tree t -> Tree t
setCategory root tree (Node node forest)
  | root == node = Node node (tree : forest)
  | True = Node node $ fmap (setCategory root tree) forest

pureCreateCategory :: Category -> Category -> State NewsCategory ()
pureCreateCategory root name = ST.modify $ setCategory root (Node name [])
