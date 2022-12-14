module Control.Server.Category 
  ( Handle(..)
  ) where

import Prelude as P

-- import Control.Applicative

-- import Data.Text
-- import Data.Time.Calendar.OrdinalDate

-- import Data.News
import Data.Types

data Handle m = Handle
  { hGetCategory :: m NewsCategory
  -- , hModifyCategory :: (NewsCateory -> NewsCategory) -> m ()
  --                     name        new root           new name
  , hChangeCategory :: Category -> Maybe Category -> Maybe Category -> m ()
  , hCreateCategory :: Category -> Category -> m ()
  }

-- handleCategoryCreate :: Monad m => Handle m -> Maybe Category -> Category -> m ()
-- handleCategoryCreate = error "Not implement"
