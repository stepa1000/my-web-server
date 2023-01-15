module Control.Server.Photo 
  ( Handle(..)
  ) where

import Prelude as P

import Data.Types

data Handle m = Handle
  { hPutPhoto :: Base64 -> m Photo
  , hGetPhoto :: Photo -> m (Maybe Base64)
  }
