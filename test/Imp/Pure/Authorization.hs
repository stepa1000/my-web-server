module Imp.Pure.Authorization where

import Control.Monad.Catch
import Control.Monad.State.Lazy
import Control.Server.Authorization
import Crypto.Hash
import Data.Imp.Server.Authorization (getHash)
import Data.Map
import Data.Typeable
import Data.Types
import Data.User
import Prelude as P

data DataAuthorization = DataAuthorization
  { userDB :: Map Login User
  }

type StateAuthorization = StateT Either Text DataAuthorization

pureAuthorization :: Handle StateAuthorization
pureAuthorization =
  Handle
    { hCreateUser = pureCreateUser,
      hUserList = pureUserList,
      hCheckAccount = pureGetAccount,
      hGetAccount = pureGetAccount,
      hAuthorizationFail = pureAuthorizationFail,
      hAdminCheckFail = pureAdminCheckFail,
      hCreatorNewsCheckFail = pureCreatorNewsCheckFail,
      hCatchErrorAuthorization = pureCatchErrorAuthorization
    }

pureCreateUser :: Name -> Login -> Password -> FlagMakeNews -> FlagAdmin -> StateAuthorization UserPublic
pureCreateUser name login password flagNameNews flagAdmin = do
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
                        adminUser = flagNameNews,
                        makeNewsUser = makeNewsUser
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
        adminUser = flagNameNews,
        makeNewsUser = makeNewsUser
      }

pureUserList :: OffSet -> Limit -> StateAuthorization [UserPublic]
pureUserList offSet limit =
  gets $ take limit . drop offset . fmap (user . snd) . toList

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
  gets $ fmap user . lookup login

pureAuthorizationFail :: StateAuthorization ()
pureAuthorizationFail = throwError ErrorAuthorization

pureCreatorNewsCheckFail :: StateAuthorization ()
pureCreatorNewsCheckFail = throwError ErrorCreatorNewsCheck

pureAdminCheckFail :: StateAuthorization ()
pureAdminCheckFail = throwError ErrorAdminCheck

pureCatchErrorAuthorization :: forall a. StateAuthorization a -> (ErrorAuthorization -> StateAuthorization a) -> StateAuthorization a
pureCatchErrorAuthorization = catchError
