{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module NewsSpec (spec) where

import Prelude as P

import Database.Beam
--import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC

--import Data.Time.Clock
--import Data.Maybe
import Data.Text
-- import qualified Data.ByteString as B
--import Data.Vector as V

import Test.Hspec 
  ({-Expectation,-} Spec, around, describe, it, shouldBe {-, shouldNotBe, shouldSatisfy-})
--import Test.QuickCheck (NonNegative (..), property, (==>))

import Data.News
--import Data.User
import Data.Types

import Control.Server.News as SN
import Data.Imp.Server.News as ISN

import Utils.News

--import qualified Control.Server.Authorization as SAuthorization
--import qualified Data.Imp.Server.Authorization as ImpSAuthorization

spec :: Spec
spec = 
  around withDatabase $
    describe "test for database news" $ do
      it "create News" $ \(h,c) -> do
        n <- handleCreateNews h loginTest nameTest newsCreateTest
        l <- handleFind h emptySearch
        _ <- BPC.runDelete c $ delete (ISN._news ISN.newsDB)
               (\a-> ISN._newsNewsName a ==. (val_ $ nameNews n))
        l `shouldBe` [n]
{-      it "debug simple search news" $ \(h,c)-> do
        n <- handleCreateNews h loginTest nameTest newsCreateTest
        l <- hSearchNewsName 3 c "nameNews"
        _ <- BPC.runDelete c $ delete (ISN._news ISN.newsDB)
               (\a-> ISN._newsNewsName a ==. (val_ $ nameNews n))
        l `shouldBe` [n]
-}
      it "simple search news full" $ \(h,c)-> do
        n <- handleCreateNews h loginTest nameTest newsCreateTest 
        l <- handleFind h $ idSearch n -- emptySearch {mName = Just "nameNews" }
        _ <- BPC.runDelete c $ delete (ISN._news ISN.newsDB)
               (\a-> ISN._newsNewsName a ==. (val_ $ nameNews n))
        l `shouldBe` [n]
      it "simple search news" $ \(h,c)-> do
        n <- handleCreateNews h loginTest nameTest newsCreateTest 
        l <- handleFind h $ emptySearch {mNewsName = Just "nameNews" }
        _ <- BPC.runDelete c $ delete (ISN._news ISN.newsDB)
               (\a-> ISN._newsNewsName a ==. (val_ $ nameNews n))
        l `shouldBe` [n]
      it "simple search news date" $ \(h,c)-> do
        n <- handleCreateNews h loginTest nameTest newsCreateTest 
        l <- handleFind h $ dateSearch n
        _ <- BPC.runDelete c $ delete (ISN._news ISN.newsDB)
               (\a-> ISN._newsNewsName a ==. (val_ $ nameNews n))
        l `shouldBe` [n]
      it "simple search news published" $ \(h,c)-> do
        n <- handleCreateNews h loginTest nameTest newsCreateTest 
        l <- handleFind h $ publicSearch n
        _ <- BPC.runDelete c $ delete (ISN._news ISN.newsDB)
               (\a-> ISN._newsNewsName a ==. (val_ $ nameNews n))
        l `shouldBe` [n]
      it "simple search news content" $ \(h,c)-> do
        n <- handleCreateNews h loginTest nameTest newsCreateTest 
        l <- handleFind h $ contentSearch n
        _ <- BPC.runDelete c $ delete (ISN._news ISN.newsDB)
               (\a-> ISN._newsNewsName a ==. (val_ $ nameNews n))
        l `shouldBe` [n]
      it "debug simple search news content" $ \(h,c)-> do
        n <- handleCreateNews h loginTest nameTest newsCreateTest 
        l <- hSearchContent 3 c (pack $ "textNews") -- handleFind h $ contentSearch n
        _ <- BPC.runDelete c $ delete (ISN._news ISN.newsDB)
               (\a-> ISN._newsNewsName a ==. (val_ $ nameNews n))
        l `shouldBe` [n]
{-      it "debug position" $ \(h,c)-> do
        n <- handleCreateNews h loginTest nameTest newsCreateTest 
        b <- debugPosition c (pack $ "textNews") -- handleFind h $ contentSearch n
        _ <- BPC.runDelete c $ delete (ISN._news ISN.newsDB)
               (\a-> ISN._newsNewsName a ==. (val_ $ nameNews n))
        b `shouldBe` B.empty
-}
      it "search news" $ \(h,c) -> do
        _ <- handleCreateNewsTestN h 10
        l <- handleFind h $ emptySearch {mNewsName = Just "nameNews5" } -- {mForString = Just "nameNews"}
        delateNews c
        (fmap textNews l) `shouldBe` ["textNews5"]
{-
addUser :: SAuthorization.Handle IO -> IO ()
addUser h = do
  _ <- SAuthorization.hCreateUser h nameTest loginTest passwordTest True False
  return ()

deleteUser :: Connection -> IO ()
deleteUser c = do
  _ <- BPC.runDelete c $ delete (ImpSAuthorization._accounts  ImpSAuthorization.accountDB) 
          (\a-> ImpSAuthorization._userLogin a ==. (val_ loginTest) )
  return ()

withDatabase :: ((SN.Handle IO, Connection) -> IO ()) -> IO ()
withDatabase f = do 
  c <- Beam.connect testDBConnect
  let hAuth = ImpSAuthorization.makeHandle configAuthorization c
  addUser hAuth
  let h = ISN.makeHandle configNews c
  delateNews c
  -- _ <- handleCreateNewsTestN h 10
  f (h,c)
  deleteUser c
  delateNews c
  Beam.close c

configNews :: ISN.Config 
configNews = ISN.Config
  { confMaxLimit = 3}

configAuthorization :: ImpSAuthorization.Config
configAuthorization = ImpSAuthorization.Config
  { ImpSAuthorization.confLimit = 3
  }

testDBConnect :: ConnectInfo
testDBConnect = defaultConnectInfo {connectUser="stepan", connectDatabase = "testDB"}

delateNews :: Connection -> IO ()
delateNews c = do
  _ <- BPC.runDelete c $ delete (ISN._news ISN.newsDB)
    (\_-> val_ True)-- (\a-> ISN._newsName a ==. (val_ $ nameNews n))
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
dateSearch n = Search
  { mDayAtSearch = Just $ dateCreationNews n
  , mDayUntil = Just $ dateCreationNews n
  , mDaySince = Just $ dateCreationNews n
  , mAuthor = Nothing -- Author
  , mCategory = Nothing --"General"
  , mNewsName = Nothing
  , mContent = Nothing
  , mForString = Nothing
  , mFlagPublished = Nothing
  , mSortBy = Nothing
  , mOffSet = Nothing
  , mLimit = Nothing
  }

publicSearch :: News -> Search
publicSearch n = Search
  { mDayAtSearch = Nothing
  , mDayUntil = Nothing
  , mDaySince = Nothing
  , mAuthor = Nothing -- Author
  , mCategory = Nothing --"General"
  , mNewsName = Nothing
  , mContent = Nothing
  , mForString = Nothing
  , mFlagPublished = Just $ publicNews n
  , mSortBy = Nothing
  , mOffSet = Nothing
  , mLimit = Nothing
  }

contentSearch :: News -> Search
contentSearch n = Search
  { mDayAtSearch = Nothing
  , mDayUntil = Nothing
  , mDaySince = Nothing
  , mAuthor = Nothing -- Author
  , mCategory = Nothing --"General"
  , mNewsName = Nothing
  , mContent = Just $ textNews n
  , mForString = Nothing
  , mFlagPublished = Nothing
  , mSortBy = Nothing
  , mOffSet = Nothing
  , mLimit = Nothing
  }

idSearch :: News -> Search
idSearch n = Search
  { mDayAtSearch = Just $ dateCreationNews n
  , mDayUntil = Just $ dateCreationNews n
  , mDaySince = Just $ dateCreationNews n
  , mAuthor = Just $ nameAuthor n -- Author
  , mCategory = Just $ categoryNews n --"General"
  , mNewsName = Just $ nameNews n
  , mContent = Just $ textNews n
  , mForString = Nothing
  , mFlagPublished = Just $ publicNews n
  , mSortBy = Nothing
  , mOffSet = Nothing
  , mLimit = Nothing
  }

newsCreateTest :: NewsCreate
newsCreateTest = NewsCreate 
  { nameNewsCreate = pack $ "nameNews"
  , categoryNewsCreate = pack $ "General"
  , textNewsCreate = pack $ "textNews"
  , photoNewsCreate = V.empty
  , newPhotoNewsCreate = V.empty
  , publicNewsCreate = False
  }

newsCreateN :: Int -> [NewsCreate]
newsCreateN i = fmap f [0..i]
  where
    f j = NewsCreate 
      { nameNewsCreate = pack $ "nameNews" P.++ (show j)
      , categoryNewsCreate = pack $ "General"
      , textNewsCreate = pack $ "textNews" P.++ (show j)
      , photoNewsCreate = V.empty
      , newPhotoNewsCreate = V.empty
      , publicNewsCreate = False
      }
-}
