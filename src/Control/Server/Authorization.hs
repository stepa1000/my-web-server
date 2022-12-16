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
  , hAuthorizationFail :: m ()
  , hAdminCheckFail :: m ()
  , hCretorNewsCheckFail :: m ()
  }

handleWithAccount :: Monad m => Handle m -> Logined -> (UserPublic -> m a) -> m (Maybe a)
handleWithAccount h l f = do
  mu <- handleCheckAccount h l
  case mu of
    (Just u) -> Just <$> f u
    _ -> do
      hAuthorizationFail h
      return Nothing

handleCheckAccountStrong :: Monad m => Handle m -> Logined -> m (Maybe UserPublic)
handleCheckAccountStrong h l = do
  mu <- handleCheckAccount h l
  case mu of
    (Just u) -> return $ Just u
    _ -> do
      hAuthorizationFail
      return Nothing

handleCheckAccount :: Handle m -> Logined -> m (Maybe UserPublic)
handleCheckAccount h logined 
  = hCheckAccount h 
    (loginLogined logined) 
    (passwordLogined logined )
