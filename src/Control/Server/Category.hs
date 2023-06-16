module Control.Server.Category
  ( Handle (..),
    handleCreateCategory,
  )
where

import Data.Foldable
import Data.Monoid
import Data.Types
import Prelude as P

data Handle m = Handle
  { hGetCategory :: m NewsCategory,
    --                     name        new root           new name
    hChangeCategory :: Category -> Maybe Category -> Maybe Category -> m (),
    --                    root         name
    hCreateCategory :: Category -> Category -> m (),
    hCreateCategoryWarn :: Category -> m ()
  }

handleFindCategory :: Monad m => Handle m -> Category -> m Bool
handleFindCategory handle category = do
  newsCategory <- hGetCategory handle
  return $ getAny $ foldMap (\name -> Any $ category == name) newsCategory

handleCreateCategory :: Monad m => Handle m -> Category -> Category -> m ()
handleCreateCategory handle root name = do
  foundCategory <- handleFindCategory handle name
  if foundCategory
    then hCreateCategoryWarn handle name
    else hCreateCategory handle root name
