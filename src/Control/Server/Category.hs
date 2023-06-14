module Control.Server.Category
  ( Handle (..),
  )
where

import Data.Types
import Prelude as P

data Handle m = Handle
  { hGetCategory :: m NewsCategory,
    --                     name        new root           new name
    hChangeCategory :: Category -> Maybe Category -> Maybe Category -> m (),
    --                    root         name
    hCreateCategory :: Category -> Category -> m ()
  }
