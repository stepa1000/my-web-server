module Control.Server.Authorization where

import Prelude as P

import Control.Applicative

import Data.Text
import Data.Time.Calendar.OrdinalDate

import Data.News
import Data.User
import Data.Types

data Handle m = Handle 
  { hCreateUser :: Name -> Login -> Password -> FlagMakeNews -> FlagAdmin -> m ()
  , hUserList :: m [UserPublic]
  , hCheckAccount :: Login -> Password -> m Bool
  }
