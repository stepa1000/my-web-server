{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module AuthorizationSpec (spec) where

import Prelude as P

import GHC.Generics

import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Conduit

import Control.Applicative

import Data.Text
import Data.ByteString
import Data.Binary
import Data.ByteArray

import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Maybe

import Crypto.Hash
import Crypto.Hash.IO

import Test.Hspec 
  (Expectation, Spec, around, describe, it, shouldBe, shouldNotBe, shouldSatisfy)
import Test.QuickCheck (NonNegative (..), property, (==>))

import Data.News
import Data.User
import Data.Types

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
        BPC.runDelete c $ delete (ImpSAuthorization._accounts  ImpSAuthorization.accountDB) 
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
        SAuthorization.hCreateUser h name login password fmn fa
        lup <- SAuthorization.hUserList h 0 3
        BPC.runDelete c $ delete (ImpSAuthorization._accounts  ImpSAuthorization.accountDB) 
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
        SAuthorization.hCreateUser h name login password fmn fa
        up <- fromJust <$> SAuthorization.hCheckAccount h login password
        BPC.runDelete c $ delete (ImpSAuthorization._accounts  ImpSAuthorization.accountDB) 
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
        SAuthorization.hCreateUser h name login password fmn fa
        up <- fromJust <$> SAuthorization.hGetAccount h login
        BPC.runDelete c $ delete (ImpSAuthorization._accounts  ImpSAuthorization.accountDB) 
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
  (h,c) <- ImpSAuthorization.makeHandle configAuthorization
  f (h,c)
  Beam.close c

configAuthorization = ImpSAuthorization.Config
  { ImpSAuthorization.confConnectInfo = testDBConnect
  , ImpSAuthorization.confLimit = 3
  }

testDBConnect = defaultConnectInfo {connectUser="stepan", connectDatabase = "testDB"}
