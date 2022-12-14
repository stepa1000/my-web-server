module Control.Server.Photo 
  ( Handle(..)
  ) where

import Prelude as P

-- import Control.Applicative

-- import Data.Text
-- import Data.ByteString as B
-- import Data.Time.Calendar.OrdinalDate

-- import Data.News
-- import Data.User
import Data.Types

data Handle m = Handle
  { hPutPhoto :: Base64 -> m Photo
  , hGetPhoto :: Photo -> m (Maybe Base64)
  }
