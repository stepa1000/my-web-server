{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

module Control.Server.Authorization
  ( Handle (..),
    ErrorAuthorization (..),
    handleWithAccount,
    handleCheckAccountStrong,
    handleCheckAccount,
    handleCatchErrorAuthorization,
    handleCreatInitAdmin,
  )
where

import Control.Monad.Catch
import Data.Typeable
import Data.Types
import Data.User
import Prelude as P

data Handle m = Handle
  { hCreateUser :: Name -> Login -> Password -> FlagMakeNews -> FlagAdmin -> m UserPublic,
    hUserList :: OffSet -> Limit -> m [UserPublic],
    hCheckAccount :: Login -> Password -> m (Maybe UserPublic),
    hGetAccount :: Login -> m (Maybe UserPublic),
    hAuthorizationFail :: m (),
    hAdminCheckFail :: m (),
    hCreatorNewsCheckFail :: m (),
    hCatchErrorAuthorization :: forall a. m a -> (ErrorAuthorization -> m a) -> m a
  }

data ErrorAuthorization
  = ErrorAuthorization
  | ErrorAdminCheck
  | ErrorCreatorNewsCheck
  deriving (Typeable, Show, Eq, Exception)

handleCreatInitAdmin :: Monad m => Handle m -> Login -> Password -> FlagMakeNews -> m (Maybe UserPublic)
handleCreatInitAdmin auth login password flagMNews = do
  lUser <- hUserList auth 0 0
  case lUser of
    [] -> Just <$> hCreateUser auth login login password flagMNews True
    _ -> return Nothing

handleCatchErrorAuthorization :: Monad m => Handle m -> m a -> m (Either ErrorAuthorization a)
handleCatchErrorAuthorization auth mact = hCatchErrorAuthorization auth (fmap Right mact) (return . Left)

handleWithAccount :: Monad m => Handle m -> Logined -> (UserPublic -> m a) -> m (Maybe a)
handleWithAccount auth logined act = do
  mUserPublic <- handleCheckAccount auth logined
  case mUserPublic of
    (Just userPublic) -> Just <$> act userPublic
    _ -> do
      hAuthorizationFail auth
      return Nothing

-- | exist with an exception if you are not logged in
handleCheckAccountStrong :: Monad m => Handle m -> Logined -> m (Maybe UserPublic)
handleCheckAccountStrong auth logined = do
  mUserPublic <- handleCheckAccount auth logined
  case mUserPublic of
    (Just userPublic) -> return $ Just userPublic
    _ -> do
      hAuthorizationFail auth
      return Nothing

handleCheckAccount :: Handle m -> Logined -> m (Maybe UserPublic)
handleCheckAccount auth logined =
  hCheckAccount
    auth
    (loginLogined logined)
    (passwordLogined logined)
