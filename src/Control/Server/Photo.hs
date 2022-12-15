module Control.Server.Photo where

import Prelude as P

import Control.Applicative

import Data.Text
import Data.Time.Calendar.OrdinalDate

import Data.News
import Data.User
import Data.Types

data Handle m = Handle
  { hPutPhoto :: BayteString -> m Photo
  , hGetPhoto :: Photo -> m ByteString
  }
