{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AuthorizationSpec (spec) where

import qualified Control.Server.Authorization as SAuthorization
import Data.Either
import Data.Map as Map
import Data.Maybe
import Data.Time.Calendar.OrdinalDate
import Data.Types
import Data.User
import Imp.Pure.Authorization
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Utils
import Prelude as P

name :: Name
name = "name1"

login :: Login
login = "login1"

password :: Password
password = "password1"

flagMakeNews :: Bool
flagMakeNews = False

flagAdmin :: Bool
flagAdmin = False

spec :: Spec
spec =
  describe
    "server authorization"
    $ do
      it "hCreateUser" $ do
        let userCreate = fromRight undefined $ stateExeT stateAuth $ SAuthorization.hCreateUser pureAuthorization name login password flagMakeNews flagAdmin
        userCreate
          `shouldBe` ( UserPublic
                         { nameUser = name,
                           loginUser = login,
                           dateCreationUser = day,
                           adminUser = flagAdmin,
                           makeNewsUser = flagMakeNews
                         }
                     )
      it "hUserList" $ do
        lUser
          `shouldBe` [ UserPublic
                         { nameUser = name,
                           loginUser = login,
                           dateCreationUser = day,
                           adminUser = flagAdmin,
                           makeNewsUser = flagMakeNews
                         }
                     ]
      it "hCheckAccount" $ do
        checkUserCheck
          `shouldBe` Nothing
      it "hGetAccount" $ do
        checkUserGet
          `shouldBe` ( Just $
                         UserPublic
                           { nameUser = name,
                             loginUser = login,
                             dateCreationUser = day,
                             adminUser = flagAdmin,
                             makeNewsUser = flagMakeNews
                           }
                     )
  where
    lUser = fromRight undefined $ stateExeT stateAuth $ do
      _ <- SAuthorization.hCreateUser pureAuthorization name login password flagMakeNews flagAdmin
      SAuthorization.hUserList pureAuthorization 0 3
    checkUserCheck = fromRight undefined $ stateExeT stateAuth $ do
      SAuthorization.hCheckAccount pureAuthorization login password
    checkUserGet = fromRight undefined $ stateExeT stateAuth $ do
      _ <- SAuthorization.hCreateUser pureAuthorization name login password flagMakeNews flagAdmin
      SAuthorization.hGetAccount pureAuthorization login

day :: Day
day = fromMondayStartWeek 2023 17 1

stateAuth :: DataAuthorization
stateAuth = DataAuthorization Map.empty
