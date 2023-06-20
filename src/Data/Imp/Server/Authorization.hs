{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Imp.Server.Authorization
  ( makeHandle,
    Config (..),
    getHash,
  )
where

import Conduit
import qualified Control.Logger as Logger
import Control.Monad
import Control.Monad.Catch
import qualified Control.Server.Authorization as ServerAuthorization
import Crypto.Hash
import Data.Binary as Binary
import Data.ByteArray
import Data.ByteString as B
import Data.Imp.Database
import Data.Maybe
import Data.Text
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Types
import Data.User
import Data.Utils
import Data.Yaml
import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Prelude as P

newtype Config = Config
  { confLimit :: Int
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeHandle :: Logger.Handle IO -> Config -> Connection -> ServerAuthorization.Handle IO
makeHandle logger config connectDB =
  ServerAuthorization.Handle
    { ServerAuthorization.hCreateUser = hCreateUser logger connectDB,
      ServerAuthorization.hUserList = hUserList logger connectDB config,
      ServerAuthorization.hCheckAccount = hCheckAccount logger connectDB,
      ServerAuthorization.hGetAccount = hGetAccount logger connectDB,
      ServerAuthorization.hAccountCheckWarn = hAccountCheckWarning logger,
      ServerAuthorization.hAuthorizationFail = hAuthorizationFail logger,
      ServerAuthorization.hAdminCheckFail = hAdminCheckFail logger,
      ServerAuthorization.hCreatorNewsCheckFail = hCreatorNewsCheckFail logger,
      ServerAuthorization.hCatchErrorAuthorization = hCatchErrorAuthorization logger
    }

hCatchErrorAuthorization :: Logger.Handle IO -> IO a -> (ServerAuthorization.ErrorAuthorization -> IO a) -> IO a
hCatchErrorAuthorization logger act react = do
  Logger.logInfo logger "Catch error authorization"
  catch act react

hCreatorNewsCheckFail :: Logger.Handle IO -> IO ()
hCreatorNewsCheckFail logger = do
  Logger.logError logger "Creator news check"
  throwM ServerAuthorization.ErrorCreatorNewsCheck

hAdminCheckFail :: Logger.Handle IO -> IO ()
hAdminCheckFail logger = do
  Logger.logError logger "Admin check"
  throwM ServerAuthorization.ErrorAdminCheck

hAccountCheckWarning :: Logger.Handle IO -> Login -> IO ()
hAccountCheckWarning logger login = Logger.logWarning logger $ "Check Account " Logger..< login

hAuthorizationFail :: Logger.Handle IO -> IO ()
hAuthorizationFail logger = do
  Logger.logError logger "Authorization"
  throwM ServerAuthorization.ErrorAuthorization

hGetAccount :: Logger.Handle IO -> Connection -> Login -> IO (Maybe UserPublic)
hGetAccount logger connectDB login = do
  Logger.logInfo logger "Get account"
  lUser <- listStreamingRunSelect connectDB $ lookup_ (dbUser webServerDB) (primaryKey $ loginUserT login)
  return (userTToUserPublic <$> listToMaybe lUser)

hCheckAccount :: Logger.Handle IO -> Connection -> Login -> Password -> IO (Maybe UserPublic)
hCheckAccount logger connectionDB login password = do
  Logger.logInfo logger "Check account"
  l <- listStreamingRunSelect connectionDB $ lookup_ (dbUser webServerDB) (primaryKey $ loginUserT login)
  case l of
    (x : _) -> do
      let px = _userPasswordHash x
      if px == getHash password
        then return $ Just $ userTToUserPublic x
        else return Nothing
    [] -> return Nothing

hUserList :: Logger.Handle IO -> Connection -> Config -> OffSet -> Limit -> IO [UserPublic]
hUserList logger connectionDB config offset limit' = do
  Logger.logInfo logger "Get user list"
  lUserPublic <-
    listStreamingRunSelect connectionDB $
      select $
        limit_ (toInteger limit) $
          offset_ (toInteger offset) $
            orderBy_ (asc_ . _userLogin) $
              all_ (dbUser webServerDB)
  return $ fmap userTToUserPublic lUserPublic
  where
    limit =
      if (limit' > confLimit config) || (limit' <= 0)
        then confLimit config
        else limit'

userTToUserPublic ::
  ( Columnar f Day ~ Day,
    Columnar f Name ~ Text,
    Columnar f FlagAdmin ~ Bool
  ) =>
  UserT f ->
  UserPublic
userTToUserPublic userT =
  UserPublic
    { nameUser = _userName userT,
      loginUser = _userLogin userT,
      dateCreationUser = _userDateCreation userT,
      adminUser = _userAdmin userT,
      makeNewsUser = _userMakeNews userT
    }

hCreateUser :: Logger.Handle IO -> Connection -> Name -> Login -> Password -> FlagMakeNews -> FlagAdmin -> IO UserPublic
hCreateUser logger connectDB name login password flagMNews flagAdmin = do
  Logger.logInfo logger "Create user"
  (UTCTime day _) <- getCurrentTime
  errorSQL <-
    BPC.runInsert connectDB $
      insert (dbUser webServerDB) $
        insertValues
          [ UserT
              { _userName = name,
                _userLogin = login,
                _userPasswordHash = getHash password,
                _userDateCreation = day,
                _userAdmin = flagAdmin,
                _userMakeNews = flagMNews
              }
          ]
  when (errorSQL == 23505) $ Logger.logWarning logger $ "Unique violation for: " Logger..< login
  return $
    UserPublic
      { nameUser = name,
        loginUser = login,
        dateCreationUser = day,
        adminUser = flagAdmin,
        makeNewsUser = flagMNews
      }

getHash :: Text -> ByteString
getHash = convert . hashlazy @SHA256 . Binary.encode

loginUserT :: Login -> UserTId
loginUserT login =
  UserT
    { _userName = undefined,
      _userLogin = login,
      _userPasswordHash = undefined,
      _userDateCreation = undefined,
      _userAdmin = undefined,
      _userMakeNews = undefined
    }
