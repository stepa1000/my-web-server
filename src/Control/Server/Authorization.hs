module Control.Server.Authorization where

import Prelude as P

import Control.Applicative

import Data.Text
import Data.Time.Calendar.OrdinalDate

import Data.News
import Data.User
import Data.Types

data Handle m = Handle 
  { hCreateUser :: Name -> Login -> Password -> FlagMakeNews -> FlagAdmin -> m UserPublic
  , hUserList :: m [UserPublic]
  , hCheckAccount :: Login -> Password -> m (Maybe UserPublic)
  , hGetAccount :: Login -> m (Maybe UserPublic)
  }

-- handleForAccount :: Handle m -> Logined -> m a -> m a -> m a

handleCheckAccount :: Handle m -> Logined -> m (Maybe UserPublic)
handleCheckAccount h logined 
  = hCheckAccount h 
    (loginLogined logined) 
    (passwordLogined logined )
