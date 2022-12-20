{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Imp.Server.Authorization 
  ( makeHandle
  , Config(..)
  , ErrorAuthorization(..)
  , UserT(..)
  , UserTId
  , UserId
  , AccountDB(..)
  , accountDB
  )
  where

import Prelude as P

import GHC.Generics

import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Conduit

import Control.Applicative
import Control.Monad.Catch

import Data.Text
import Data.ByteString
import Data.Binary
import Data.ByteArray

import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Crypto.Hash
import Crypto.Hash.IO

import Data.Maybe
import Data.Typeable

import Data.News
import Data.User
import Data.Types

import qualified Control.Server.Authorization as ServerAuthorization

data Config = Config
  { confConnectInfo :: ConnectInfo
  , confLimit :: Int
  } deriving Show

data ErrorAuthorization 
  = ErrorAuthorization
  | ErrorAdminCheck
  | ErrorCreatorNewsCheck
  deriving (Typeable, Show, Eq, Exception)

withServerAuthorization :: Config -> (ServerAuthorization.Handle IO -> IO a) -> IO a
withServerAuthorization c f = error "Not implement"

makeHandle :: Config -> IO (ServerAuthorization.Handle IO, Connection)
makeHandle config = do
  c <- Beam.connect $ confConnectInfo config
  return 
    ( ServerAuthorization.Handle 
      { ServerAuthorization.hCreateUser = hCreateUser c config
      , ServerAuthorization.hUserList = hUserList c config
      , ServerAuthorization.hCheckAccount = hCheckAccount c
      , ServerAuthorization.hGetAccount = hGetAccount c
      , ServerAuthorization.hAuthorizationFail = hAuthorizationFail
      , ServerAuthorization.hAdminCheckFail = hAdminCheckFail
      , ServerAuthorization.hCreatorNewsCheckFail = hCreatorNewsCheckFail
      }
    , c
    )

hCreatorNewsCheckFail :: IO ()
hCreatorNewsCheckFail = do
  throwM ErrorCreatorNewsCheck

hAdminCheckFail :: IO ()
hAdminCheckFail = do
  throwM ErrorAdminCheck

hAuthorizationFail :: IO ()
hAuthorizationFail = do
  throwM ErrorAuthorization

hGetAccount :: Connection -> Login -> IO (Maybe UserPublic)
hGetAccount c login = do
  l <- listStreamingRunSelect c $ lookup_ (_accounts accountDB) (primaryKey $ UserT {_loginUserT = login})
  return $ fmap userTToUserPublic $ listToMaybe l

hCheckAccount :: Connection -> Login -> Password -> IO (Maybe UserPublic)
hCheckAccount c login p = do
  l <- listStreamingRunSelect c $ lookup_ (_accounts accountDB) (primaryKey $ UserT {_loginUserT = login})
  case l of
    (x:_) -> do
      let px = _passwordHashUserT x
      if px == (getHash p)
        then return $ Just $ userTToUserPublic x
        else return $ Nothing
    [] -> return Nothing

hUserList :: Connection -> Config -> OffSet -> Limit -> IO [UserPublic]
hUserList conn config offset limit' = do -- error "Not implement"
  lut <- listStreamingRunSelect conn $ select $ limit_ (toInteger limit) $ offset_ (toInteger offset) $
    orderBy_ (asc_ . _loginUserT) $ all_ (_accounts accountDB) 
  return $ fmap userTToUserPublic lut
  where
    limit = if limit' > (confLimit config) 
      then (confLimit config)
      else limit'

userTToUserPublic ut = UserPublic
      { nameUser = _nameUserT ut
      , loginUser = _loginUserT ut
      , dateCreationUser = _dateCreationUserT ut
      , adminUser = _adminUserT ut
      , makeNewsUser = _makeNewsUserT ut
      }

listStreamingRunSelect :: FromBackendRow Postgres a => Connection -> SqlSelect Postgres a -> IO [a] 
listStreamingRunSelect c sqls = 
  runConduitRes $ (streamingRunSelect c sqls
       ) .| sinkList

hCreateUser :: Connection -> Config ->  Name -> Login -> Password -> FlagMakeNews -> FlagAdmin -> IO UserPublic
hCreateUser c conf name login password flagMN flagA = do
  l <- listStreamingRunSelect c $ lookup_ (_accounts accountDB) (primaryKey $ UserT {_loginUserT = login})
       -- ) .| sinkList -- (sinkVectorN )
  case l of
    [] -> do 
      (UTCTime day _) <- getCurrentTime
      BPC.runInsert c $ insert (_accounts accountDB) $ insertValues
        [ UserT 
          { _nameUserT = name
          , _loginUserT = login
          , _passwordHashUserT = getHash password
          , _dateCreationUserT = day
          , _adminUserT = flagA
          , _makeNewsUserT = flagMN
          }
        ]
      return $ UserPublic
        { nameUser = name
        , loginUser = login
        , dateCreationUser = day
        , adminUser = flagA
        , makeNewsUser = flagMN
        }
    (x:_) -> do
      return $ UserPublic
        { nameUser = _nameUserT x
        , loginUser = _loginUserT x
        , dateCreationUser = _dateCreationUserT x
        , adminUser = _adminUserT x
        , makeNewsUser = _makeNewsUserT x
        }

getHash :: Text -> ByteString
getHash = convert . hashlazy  @SHA256 . encode

data UserT f = UserT
  { _nameUserT :: Columnar f Name
  , _loginUserT :: Columnar f Login
  , _passwordHashUserT :: Columnar f ByteString
  , _dateCreationUserT :: Columnar f Day
  , _adminUserT :: Columnar f FlagAdmin
  , _makeNewsUserT :: Columnar f FlagMakeNews
  } deriving (Generic, Beamable)

type UserTId = UserT Identity
type UserId = PrimaryKey UserT Identity

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Login) 
    deriving (Generic, Beamable)
  primaryKey = UserId . _loginUserT

data AccountDB f = AccountDB
  { _accounts :: f (TableEntity UserT) } 
  deriving (Generic, Database be)

accountDB :: DatabaseSettings be AccountDB
accountDB = defaultDbSettings 
