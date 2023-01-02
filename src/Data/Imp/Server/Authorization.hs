{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Imp.Server.Authorization 
  ( makeHandle
  , Config(..)
  -- , ErrorAuthorization(..)
  , UserT(..)
  , UserTId
  , UserId
  , AccountDB(..)
  , accountDB
  )
  where

import Prelude as P

import Servant.API
import Servant.Server as Servant

import GHC.Generics

import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Conduit

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Error

import Data.Text
import Data.ByteString as B
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
import Data.Utils

import qualified Control.Server.Authorization as ServerAuthorization

data Config = Config
  { -- confConnectInfo :: ConnectInfo
   confLimit :: Int
  } deriving Show
{-
data ErrorAuthorization 
  = ErrorAuthorization
  | ErrorAdminCheck
  | ErrorCreatorNewsCheck
  deriving (Typeable, Show, Eq, Exception)

Name -> Login -> Password -> FlagMakeNews -> FlagAdmin
-}
withServerAuthorization :: Config -> (ServerAuthorization.Handle IO -> IO a) -> IO a
withServerAuthorization c f = error "Not implement"

makeHandleServant :: Config -> Connection -> ServerAuthorization.Handle (Servant.Handler)
makeHandleServant config c = 
  ServerAuthorization.Handle
    { ServerAuthorization.hCreateUser = \n l p fmn fa -> liftIO $ hCreateUser c config n l p fmn fa
    , ServerAuthorization.hUserList = \o l-> liftIO $ hUserList c config o l
    , ServerAuthorization.hCheckAccount = \l p-> liftIO $ hCheckAccount c l p
    , ServerAuthorization.hGetAccount = \l-> liftIO $ hGetAccount c l
    , ServerAuthorization.hAuthorizationFail = hAuthorizationFailServant
    , ServerAuthorization.hAdminCheckFail = hAdminCheckFailServant
    , ServerAuthorization.hCreatorNewsCheckFail = hCreatorNewsCheckFailServant
    --, ServerAuthorization.hCatchErrorAuthorization = hCatchErrorAuthorizationServant
    }

hAuthorizationFailServant :: Servant.Handler ()
hAuthorizationFailServant = do
  throwError $ ServerError
    { errHTTPCode = 400
    , errReasonPhrase = "fail authorization"
    , errBody = fromStrict $ B.empty
    , errHeaders = []
    }
{-
hCatchErrorAuthorizationServant :: Servant.Handler ()
hCatchErrorAuthorizationServant = do
  throwError $ ServerError
    { errHTTPCode = 400
    , errReasonPhrase = "creator not maker news"
    , errBody = fromStrict $ B.empty
    , errHeaders = []
    }
-}
hCreatorNewsCheckFailServant :: Servant.Handler ()
hCreatorNewsCheckFailServant = do
  throwError $ ServerError
    { errHTTPCode = 400
    , errReasonPhrase = "creator not maker news"
    , errBody = fromStrict $ B.empty
    , errHeaders = []
    }

hAdminCheckFailServant :: Servant.Handler ()
hAdminCheckFailServant = do
  throwError $ ServerError
    { errHTTPCode = 404
    , errReasonPhrase = ""
    , errBody = fromStrict $ B.empty
    , errHeaders = []
    }

makeHandle :: Config -> Connection -> ServerAuthorization.Handle IO
makeHandle config c =
  ServerAuthorization.Handle 
    { ServerAuthorization.hCreateUser = hCreateUser c config
    , ServerAuthorization.hUserList = hUserList c config
    , ServerAuthorization.hCheckAccount = hCheckAccount c
    , ServerAuthorization.hGetAccount = hGetAccount c
    , ServerAuthorization.hAuthorizationFail = hAuthorizationFail
    , ServerAuthorization.hAdminCheckFail = hAdminCheckFail
    , ServerAuthorization.hCreatorNewsCheckFail = hCreatorNewsCheckFail
    -- , ServerAuthorization.hCatchErrorAuthorization = hCatchErrorAuthorization
    }


hCatchErrorAuthorization :: IO a -> (ServerAuthorization.ErrorAuthorization -> IO a) -> IO a
hCatchErrorAuthorization ma c = catch ma c

hCreatorNewsCheckFail :: IO ()
hCreatorNewsCheckFail = do
  throwM ServerAuthorization.ErrorCreatorNewsCheck

hAdminCheckFail :: IO ()
hAdminCheckFail = do
  throwM ServerAuthorization.ErrorAdminCheck

hAuthorizationFail :: IO ()
hAuthorizationFail = do
  throwM ServerAuthorization.ErrorAuthorization

hGetAccount :: Connection -> Login -> IO (Maybe UserPublic)
hGetAccount c login = do
  l <- listStreamingRunSelect c $ lookup_ (_accounts accountDB) (primaryKey $ UserT {_userLogin = login})
  return $ fmap userTToUserPublic $ listToMaybe l

hCheckAccount :: Connection -> Login -> Password -> IO (Maybe UserPublic)
hCheckAccount c login p = do
  l <- listStreamingRunSelect c $ lookup_ (_accounts accountDB) (primaryKey $ UserT {_userLogin = login})
  case l of
    (x:_) -> do
      let px = _userPasswordHash x
      if px == (getHash p)
        then return $ Just $ userTToUserPublic x
        else return $ Nothing
    [] -> return Nothing

hUserList :: Connection -> Config -> OffSet -> Limit -> IO [UserPublic]
hUserList conn config offset limit' = do -- error "Not implement"
  lut <- listStreamingRunSelect conn $ select $ limit_ (toInteger limit) $ offset_ (toInteger offset) $
    orderBy_ (asc_ . _userLogin) $ all_ (_accounts accountDB) 
  return $ fmap userTToUserPublic lut
  where
    limit = if limit' > (confLimit config) 
      then (confLimit config)
      else limit'

userTToUserPublic ut = UserPublic
      { nameUser = _userName ut
      , loginUser = _userLogin ut
      , dateCreationUser = _userDateCreation ut
      , adminUser = _userAdmin ut
      , makeNewsUser = _userMakeNews ut
      }

hCreateUser :: Connection -> Config ->  Name -> Login -> Password -> FlagMakeNews -> FlagAdmin -> IO UserPublic
hCreateUser c conf name login password flagMN flagA = do
  l <- listStreamingRunSelect c $ lookup_ (_accounts accountDB) (primaryKey $ UserT {_userLogin = login})
       -- ) .| sinkList -- (sinkVectorN )
  case l of
    [] -> do 
      (UTCTime day _) <- getCurrentTime
      BPC.runInsert c $ insert (_accounts accountDB) $ insertValues
        [ UserT 
          { _userName = name
          , _userLogin = login
          , _userPasswordHash = getHash password
          , _userDateCreation = day
          , _userAdmin = flagA
          , _userMakeNews = flagMN
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
        { nameUser = _userName x
        , loginUser = _userLogin x
        , dateCreationUser = _userDateCreation x
        , adminUser = _userAdmin x
        , makeNewsUser = _userMakeNews x
        }

getHash :: Text -> ByteString
getHash = convert . hashlazy  @SHA256 . encode

data UserT f = UserT
  { _userName :: Columnar f Name
  , _userLogin :: Columnar f Login
  , _userPasswordHash :: Columnar f ByteString
  , _userDateCreation :: Columnar f Day
  , _userAdmin :: Columnar f FlagAdmin
  , _userMakeNews :: Columnar f FlagMakeNews
  } deriving (Generic, Beamable)

type UserTId = UserT Identity
type UserId = PrimaryKey UserT Identity

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Login) 
    deriving (Generic, Beamable)
  primaryKey = UserId . _userLogin

data AccountDB f = AccountDB
  { _accounts :: f (TableEntity UserT) } 
  deriving (Generic, Database be)

accountDB :: DatabaseSettings be AccountDB
accountDB = defaultDbSettings 
