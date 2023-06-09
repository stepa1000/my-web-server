{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AuthorizationSpec (spec) where

import Control.Monad.State.Lazy
import qualified Control.Server.Authorization as SAuthorization
import Data.Either
import Data.Map as Map
import Data.Maybe
import Data.Time.Calendar.OrdinalDate
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

spec :: Spec
spec =
  describe
    "server authorization"
    $ do
      it "hCreateUser" $ do
        let name = "name1"
        let login = "login1"
        let password = "password1"
        let flagMakeNews = False
        let flagAdmin = False
        userCreate <- return $ fromRight undefined $ stateExeT stateAuth $ SAuthorization.hCreateUser pureAuthorization name login password flagMakeNews flagAdmin
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
        let name = "name1"
        let login = "login1"
        let password = "password1"
        let flagMakeNews = False
        let flagAdmin = False
        lUser <- return $ fromRight undefined $ stateExeT stateAuth $ do
          _ <- SAuthorization.hCreateUser pureAuthorization name login password flagMakeNews flagAdmin
          SAuthorization.hUserList pureAuthorization 0 3
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
        let name = "name1"
        let login = "login1"
        let password = "password1"
        let flagMakeNews = False
        let flagAdmin = False
        checkUser <- return $ fromRight undefined $ stateExeT stateAuth $ do
          _ <- SAuthorization.hCreateUser pureAuthorization name login password flagMakeNews flagAdmin
          SAuthorization.hCheckAccount pureAuthorization login password
        checkUser
          `shouldBe` ( Just $
                         UserPublic
                           { nameUser = name,
                             loginUser = login,
                             dateCreationUser = day,
                             adminUser = flagAdmin,
                             makeNewsUser = flagMakeNews
                           }
                     )
      it "hGetAccount" $ do
        let name = "name1"
        let login = "login1"
        let password = "password1"
        let flagMakeNews = False
        let flagAdmin = False
        checkUser <- return $ fromRight undefined $ stateExeT stateAuth $ do
          _ <- SAuthorization.hCreateUser pureAuthorization name login password flagMakeNews flagAdmin
          SAuthorization.hGetAccount pureAuthorization login
        checkUser
          `shouldBe` ( Just $
                         UserPublic
                           { nameUser = name,
                             loginUser = login,
                             dateCreationUser = day,
                             adminUser = flagAdmin,
                             makeNewsUser = flagMakeNews
                           }
                     )

day :: Day
day = fromMondayStartWeek 2023 17 1

stateAuth :: DataAuthorization
stateAuth = DataAuthorization Map.empty
