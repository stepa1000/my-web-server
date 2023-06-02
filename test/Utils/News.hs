{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.News
  ( addUser,
    deleteUser,
    withDatabase,
    configNews,
    configAuthorization,
    testDBConnect,
    delateNews,
    handleCreateNewsTestN,
    handleCreateNewsTest,
    loginTest,
    nameTest,
    passwordTest,
    dateSearch,
    publicSearch,
    contentSearch,
    idSearch,
    newsCreateTest,
    newsCreateN,
  )
where

import qualified Control.Server.Authorization as SAuthorization
import Control.Server.News as SN
import Data.Config
import Data.Imp.Database
import Data.Imp.Server
import qualified Data.Imp.Server.Authorization as ImpSAuthorization
import Data.Imp.Server.News as ISN
import Data.Logger.Impl
import Data.News
import Data.Text
import Data.Types
import Data.Vector as V
import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Prelude as P

addUser :: SAuthorization.Handle IO -> IO ()
addUser h = do
  _ <- SAuthorization.hCreateUser h nameTest loginTest passwordTest True False
  return ()

deleteUser :: Connection -> IO ()
deleteUser c = do
  _ <-
    BPC.runDelete c $
      delete
        (dbUser webServerDB)
        (\a -> _userLogin a ==. (val_ loginTest))
  return ()

withDatabase :: ((SN.Handle IO, Connection) -> IO ()) -> IO ()
withDatabase act = do
  serverConfig <- getServerSettingsTest
  c <- Beam.connect $ confConnectionInfo serverConfig
  withPreConf (confLogger serverConfig) $ \logger -> do
    let hAuth = ImpSAuthorization.makeHandle logger configAuthorization c
    addUser hAuth
    let h = ISN.makeHandle logger configNews c
    delateNews c
    act (h, c)
    deleteUser c
    delateNews c
    Beam.close c

configNews :: ISN.Config
configNews =
  ISN.Config
    { confMaxLimit = 3
    }

configAuthorization :: ImpSAuthorization.Config
configAuthorization =
  ImpSAuthorization.Config
    { ImpSAuthorization.confLimit = 3
    }

testDBConnect :: ConnectInfo
testDBConnect = defaultConnectInfo {connectUser = "stepan", connectDatabase = "testDB"}

delateNews :: Connection -> IO ()
delateNews c = do
  _ <-
    BPC.runDelete c $
      delete
        (dbNews webServerDB)
        (\_ -> val_ True)
  return ()

handleCreateNewsTestN :: Monad m => SN.Handle m -> Int -> m [News]
handleCreateNewsTestN h i = handleCreateNewsTest h (newsCreateN i)

handleCreateNewsTest :: Monad m => SN.Handle m -> [NewsCreate] -> m [News]
handleCreateNewsTest h = P.mapM (handleCreateNews h loginTest nameTest)

loginTest :: Login
loginTest = "loginTest"

nameTest :: Name
nameTest = "nameTest"

passwordTest :: Password
passwordTest = "testPassword"

dateSearch :: News -> Search
dateSearch n =
  Search
    { mDayAtSearch = Just $ dateCreationNews n,
      mDayUntil = Just $ dateCreationNews n,
      mDaySince = Just $ dateCreationNews n,
      mAuthor = Nothing, -- Author
      mCategory = Nothing, -- "General"
      mNewsUUID = Nothing,
      mNewsName = Nothing,
      mContent = Nothing,
      mForString = Nothing,
      mFlagPublished = Nothing,
      mSortBy = Nothing,
      mOffSet = Nothing,
      mLimit = Nothing
    }

publicSearch :: News -> Search
publicSearch n =
  Search
    { mDayAtSearch = Nothing,
      mDayUntil = Nothing,
      mDaySince = Nothing,
      mAuthor = Nothing, -- Author
      mCategory = Nothing, -- "General"
      mNewsUUID = Nothing,
      mNewsName = Nothing,
      mContent = Nothing,
      mForString = Nothing,
      mFlagPublished = Just $ publicNews n,
      mSortBy = Nothing,
      mOffSet = Nothing,
      mLimit = Nothing
    }

contentSearch :: News -> Search
contentSearch n =
  Search
    { mDayAtSearch = Nothing,
      mDayUntil = Nothing,
      mDaySince = Nothing,
      mAuthor = Nothing, -- Author
      mCategory = Nothing, -- "General"
      mNewsUUID = Nothing,
      mNewsName = Nothing,
      mContent = Just $ textNews n,
      mForString = Nothing,
      mFlagPublished = Nothing,
      mSortBy = Nothing,
      mOffSet = Nothing,
      mLimit = Nothing
    }

idSearch :: News -> Search
idSearch n =
  Search
    { mDayAtSearch = Just $ dateCreationNews n,
      mDayUntil = Just $ dateCreationNews n,
      mDaySince = Just $ dateCreationNews n,
      mAuthor = Just $ nameAuthor n, -- Author
      mCategory = Just $ categoryNews n, -- "General"
      mNewsUUID = Nothing,
      mNewsName = Just $ nameNews n,
      mContent = Just $ textNews n,
      mForString = Nothing,
      mFlagPublished = Just $ publicNews n,
      mSortBy = Nothing,
      mOffSet = Nothing,
      mLimit = Nothing
    }

newsCreateTest :: NewsCreate
newsCreateTest =
  NewsCreate
    { nameNewsCreate = pack $ "nameNews",
      categoryNewsCreate = pack $ "General",
      textNewsCreate = pack $ "textNews",
      photoNewsCreate = V.empty,
      newPhotoNewsCreate = V.empty,
      publicNewsCreate = False
    }

newsCreateN :: Int -> [NewsCreate]
newsCreateN i = fmap f [0 .. i]
  where
    f j =
      NewsCreate
        { nameNewsCreate = pack $ "nameNews" P.++ (show j),
          categoryNewsCreate = pack $ "General",
          textNewsCreate = pack $ "textNews" P.++ (show j),
          photoNewsCreate = V.empty,
          newPhotoNewsCreate = V.empty,
          publicNewsCreate = False
        }
