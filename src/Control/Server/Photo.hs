module Control.Server.Photo
  ( Handle (..),
  )
where

import Data.Types
import Prelude as P

data Handle m = Handle
  { hPutPhoto :: Base64 -> m Photo,
    hGetPhoto :: Photo -> m (Maybe Base64)
  }
