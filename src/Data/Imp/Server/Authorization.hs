{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Data.Imp.Server.Authorization where

import Prelude as P

import GHC.Generics

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Conduit as BPC
import Conduit

import Control.Applicative

import Data.Text
import Data.ByteString
import Data.Binary
import Data.ByteArray

import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Crypto.Hash
import Crypto.Hash.IO

import Data.News
import Data.User
import Data.Types

import qualified Control.Server.Authorization as ServerAuthorization

data Config = Config
  { confConnectInfo :: ConnectInfo
  , confMaxOffSet :: Int
  } deriving Show

withServerAuthorization :: Config -> (ServerAuthorization.Handle IO -> IO a) -> IO a
withServerAuthorization c f = error "Not implement"

hUserList :: Connection -> Config -> OffSet -> Limit -> IO [UserPublic]
hUserList conn config offset limit = error "Not implement"

hCreateUser :: Connection -> Config ->  Name -> Login -> Password -> FlagMakeNews -> FlagAdmin -> IO UserPublic
hCreateUser c conf name login password flagMN flagA = do
  l <- runConduitRes @IO $ (streamingRunSelect c $ lookup_ (_accounts accountDB) (primaryKey $ UserT {_loginUserT = login})
       ) .| sinkList -- (sinkVectorN )
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
