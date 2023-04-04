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
    UserT (..),
    UserTId,
    UserId,
  )
where

import Conduit
import qualified Control.Logger as Logger
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
makeHandle hl config c =
  ServerAuthorization.Handle
    { ServerAuthorization.hCreateUser = hCreateUser hl c,
      ServerAuthorization.hUserList = hUserList hl c config,
      ServerAuthorization.hCheckAccount = hCheckAccount hl c,
      ServerAuthorization.hGetAccount = hGetAccount hl c,
      ServerAuthorization.hAuthorizationFail = hAuthorizationFail hl,
      ServerAuthorization.hAdminCheckFail = hAdminCheckFail hl,
      ServerAuthorization.hCreatorNewsCheckFail = hCreatorNewsCheckFail hl,
      ServerAuthorization.hCatchErrorAuthorization = hCatchErrorAuthorization hl
    }

hCatchErrorAuthorization :: Logger.Handle IO -> IO a -> (ServerAuthorization.ErrorAuthorization -> IO a) -> IO a
hCatchErrorAuthorization hl act react = do
  Logger.logInfo hl "Catch error authorization"
  catch act react

hCreatorNewsCheckFail :: Logger.Handle IO -> IO ()
hCreatorNewsCheckFail hl = do
  Logger.logError hl "Creator news check"
  throwM ServerAuthorization.ErrorCreatorNewsCheck

hAdminCheckFail :: Logger.Handle IO -> IO ()
hAdminCheckFail hl = do
  Logger.logError hl "Admin check"
  throwM ServerAuthorization.ErrorAdminCheck

hAuthorizationFail :: Logger.Handle IO -> IO ()
hAuthorizationFail hl = do
  Logger.logError hl "Authorization"
  throwM ServerAuthorization.ErrorAuthorization

hGetAccount :: Logger.Handle IO -> Connection -> Login -> IO (Maybe UserPublic)
hGetAccount hl c login = do
  Logger.logInfo hl "Get account"
  l <- listStreamingRunSelect c $ lookup_ (_accounts accountDB) (primaryKey $ loginUserT login)
  return (userTToUserPublic <$> listToMaybe l)

hCheckAccount :: Logger.Handle IO -> Connection -> Login -> Password -> IO (Maybe UserPublic)
hCheckAccount hl c login p = do
  Logger.logInfo hl "Check account"
  l <- listStreamingRunSelect c $ lookup_ (_accounts accountDB) (primaryKey $ loginUserT login)
  case l of
    (x : _) -> do
      let px = _userPasswordHash x
      if px == getHash p
        then return $ Just $ userTToUserPublic x
        else return Nothing
    [] -> return Nothing

hUserList :: Logger.Handle IO -> Connection -> Config -> OffSet -> Limit -> IO [UserPublic]
hUserList hl conn config offset limit' = do
  Logger.logInfo hl "Get user list"
  lut <-
    listStreamingRunSelect conn $
      select $
        limit_ (toInteger limit) $
          offset_ (toInteger offset) $
            orderBy_ (asc_ . _userLogin) $
              all_ (_accounts accountDB)
  return $ fmap userTToUserPublic lut
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
userTToUserPublic ut =
  UserPublic
    { nameUser = _userName ut,
      loginUser = _userLogin ut,
      dateCreationUser = _userDateCreation ut,
      adminUser = _userAdmin ut,
      makeNewsUser = _userMakeNews ut
    }

hCreateUser :: Logger.Handle IO -> Connection -> Name -> Login -> Password -> FlagMakeNews -> FlagAdmin -> IO UserPublic
hCreateUser hl c name login password flagMN flagA = do
  Logger.logInfo hl "Create user"
  l <- listStreamingRunSelect c $ lookup_ (_accounts accountDB) (primaryKey $ loginUserT login)
  case l of
    [] -> do
      (UTCTime day _) <- getCurrentTime
      _ <-
        BPC.runInsert c $
          insert (_accounts accountDB) $
            insertValues
              [ UserT
                  { _userName = name,
                    _userLogin = login,
                    _userPasswordHash = getHash password,
                    _userDateCreation = day,
                    _userAdmin = flagA,
                    _userMakeNews = flagMN
                  }
              ]
      return $
        UserPublic
          { nameUser = name,
            loginUser = login,
            dateCreationUser = day,
            adminUser = flagA,
            makeNewsUser = flagMN
          }
    (x : _) -> do
      return $
        UserPublic
          { nameUser = _userName x,
            loginUser = _userLogin x,
            dateCreationUser = _userDateCreation x,
            adminUser = _userAdmin x,
            makeNewsUser = _userMakeNews x
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
