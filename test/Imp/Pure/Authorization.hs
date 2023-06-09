{-# LANGUAGE RankNTypes #-}

module Imp.Pure.Authorization
  ( DataAuthorization (..),
    StateAuthorization,
    pureAuthorization,
  )
where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Server.Authorization
import Data.Bool
import Data.Either
import Data.Imp.Server.Authorization (getHash)
import Data.Map
import Data.Maybe
import Data.Time.Calendar.OrdinalDate
import Data.Types
import Data.User
import Prelude (snd, ($), (.), (==))
import qualified Prelude as P

data DataAuthorization = DataAuthorization
  { userDB :: Map Login User
  }

type StateAuthorization = StateT DataAuthorization (Either ErrorAuthorization)

pureAuthorization :: Handle StateAuthorization
pureAuthorization =
  Handle
    { hCreateUser = pureCreateUser,
      hUserList = pureUserList,
      hCheckAccount = pureCheckAccount,
      hGetAccount = pureGetAccount,
      hAuthorizationFail = pureAuthorizationFail,
      hAdminCheckFail = pureAdminCheckFail,
      hCreatorNewsCheckFail = pureCreatorNewsCheckFail,
      hCatchErrorAuthorization = pureCatchErrorAuthorization
    }

pureCreateUser :: Name -> Login -> Password -> FlagMakeNews -> FlagAdmin -> StateAuthorization UserPublic
pureCreateUser name login password flagMakeNews flagAdmin = do
  modify
    ( \(DataAuthorization userdb) ->
        DataAuthorization $
          insert
            login
            ( User
                { user =
                    UserPublic
                      { nameUser = name,
                        loginUser = login,
                        dateCreationUser = fromMondayStartWeek 2023 17 1,
                        adminUser = flagAdmin,
                        makeNewsUser = flagMakeNews
                      },
                  passwordHashUser = getHash password
                }
            )
            userdb
    )
  return $
    UserPublic
      { nameUser = name,
        loginUser = login,
        dateCreationUser = fromMondayStartWeek 2023 17 1,
        adminUser = flagAdmin,
        makeNewsUser = flagMakeNews
      }

pureUserList :: OffSet -> Limit -> StateAuthorization [UserPublic]
pureUserList offSet limit =
  gets $ P.take limit . P.drop offSet . fmap (user . snd) . toList . userDB

pureCheckAccount :: Login -> Password -> StateAuthorization (Maybe UserPublic)
pureCheckAccount login password =
  state $ \dataAuth -> (checkAccount $ userDB dataAuth, dataAuth)
  where
    checkAccount mapUser = do
      account <- lookup login mapUser
      case passwordHashUser account == getHash password of
        True -> return $ user account
        False -> Nothing

pureGetAccount :: Login -> StateAuthorization (Maybe UserPublic)
pureGetAccount login =
  gets $ fmap user . lookup login . userDB

pureAuthorizationFail :: StateAuthorization ()
pureAuthorizationFail = throwError ErrorAuthorization

pureCreatorNewsCheckFail :: StateAuthorization ()
pureCreatorNewsCheckFail = throwError ErrorCreatorNewsCheck

pureAdminCheckFail :: StateAuthorization ()
pureAdminCheckFail = throwError ErrorAdminCheck

pureCatchErrorAuthorization :: forall a. StateAuthorization a -> (ErrorAuthorization -> StateAuthorization a) -> StateAuthorization a
pureCatchErrorAuthorization = catchError
