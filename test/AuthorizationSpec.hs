{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module AuthorizationSpec (spec) where

import Prelude as P

import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC

import Data.Time.Clock
import Data.Maybe

import Test.Hspec 
  ({-Expectation,-} Spec, around, describe, it, shouldBe {-, shouldNotBe, shouldSatisfy-})
--import Test.QuickCheck (NonNegative (..), property, (==>))

--import Data.News
import Data.User
--import Data.Types

import qualified Control.Server.Authorization as SAuthorization
import qualified Data.Imp.Server.Authorization as ImpSAuthorization

spec :: Spec
spec = 
  around withDatabase $
    describe "server authorization" $ do
      it "hCreateUser" $ \ (h,c) -> do
        let name = "name1"
        let login = "login1"
        let password = "password1"
        let fmn = False
        let fa = False
        up <- SAuthorization.hCreateUser h name login password fmn fa
        _ <- BPC.runDelete c $ delete (ImpSAuthorization._accounts  ImpSAuthorization.accountDB) 
          (\a-> ImpSAuthorization._userLogin a ==. "login1")
        (UTCTime day _) <- getCurrentTime
        up `shouldBe` (UserPublic 
          { nameUser = name
          , loginUser = login
          , dateCreationUser = day
          , adminUser = fa
          , makeNewsUser = fmn 
          }
          )
      it "hUserList" $ \ (h,c) -> do
        let name = "name1"
        let login = "login1"
        let password = "password1"
        let fmn = False
        let fa = False
        _ <- SAuthorization.hCreateUser h name login password fmn fa
        lup <- SAuthorization.hUserList h 0 3
        _ <- BPC.runDelete c $ delete (ImpSAuthorization._accounts  ImpSAuthorization.accountDB) 
          (\a-> ImpSAuthorization._userLogin a ==. "login1")
        (UTCTime day _) <- getCurrentTime
        lup `shouldBe` [UserPublic 
          { nameUser = name
          , loginUser = login
          , dateCreationUser = day
          , adminUser = fa
          , makeNewsUser = fmn
          }]
      it "hCheckAccount" $ \ (h,c) -> do
        let name = "name1"
        let login = "login1"
        let password = "password1"
        let fmn = False
        let fa = False
        _ <- SAuthorization.hCreateUser h name login password fmn fa
        up <- fromJust <$> SAuthorization.hCheckAccount h login password
        _ <- BPC.runDelete c $ delete (ImpSAuthorization._accounts  ImpSAuthorization.accountDB) 
          (\a-> ImpSAuthorization._userLogin a ==. "login1")
        (UTCTime day _) <- getCurrentTime
        up `shouldBe` (UserPublic 
          { nameUser = name
          , loginUser = login
          , dateCreationUser = day
          , adminUser = fa
          , makeNewsUser = fmn 
          }
          )
      it "hGetAccount" $ \ (h,c) -> do
        let name = "name1"
        let login = "login1"
        let password = "password1"
        let fmn = False
        let fa = False
        _ <- SAuthorization.hCreateUser h name login password fmn fa
        up <- fromJust <$> SAuthorization.hGetAccount h login
        _ <- BPC.runDelete c $ delete (ImpSAuthorization._accounts  ImpSAuthorization.accountDB) 
          (\a-> ImpSAuthorization._userLogin a ==. "login1")
        (UTCTime day _) <- getCurrentTime
        up `shouldBe` (UserPublic 
          { nameUser = name
          , loginUser = login
          , dateCreationUser = day
          , adminUser = fa
          , makeNewsUser = fmn 
          }
          )
        

{-
createUserTest :: Int -> IO ()
createUserTest i = do
  let name = "name" ++ (show i)
  let login = "login" ++ (show i)
  let password = "password" ++ (show i)
  let fmn = False
  let fa = False
  SAuthorization.hCreateUser h name login password fmn fa
-}

withDatabase :: ((SAuthorization.Handle IO, Connection) -> IO ()) -> IO ()
withDatabase f = do 
  c <- Beam.connect testDBConnect
  let h = ImpSAuthorization.makeHandle configAuthorization c
  f (h,c)
  Beam.close c

configAuthorization :: ImpSAuthorization.Config
configAuthorization = ImpSAuthorization.Config
  { ImpSAuthorization.confLimit = 3
  }
--  { ImpSAuthorization.confConnectInfo = testDBConnect
--  , ImpSAuthorization.confLimit = 3
--  }

testDBConnect :: ConnectInfo
testDBConnect = defaultConnectInfo {connectUser="stepan", connectDatabase = "testDB"}
