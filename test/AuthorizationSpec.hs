{-# LANGUAGE TypeFamilies #-}

module AuthorizationSpec where

import qualified Control.Server.Authorization as SAuthorization
import Data.Config
import Data.Imp.Database as ImpSAuthorization
import Data.Imp.Server
import qualified Data.Imp.Server.Authorization as ImpSAuthorization
import Data.Logger.Impl
import Data.Maybe
import Data.Time.Clock
import Data.User
import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Test.Hspec
  ( Spec,
    aroundAll,
    describe,
    it,
    shouldBe,
  )
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
        userCreate <- stateExe stateAuth $ SAuthorization.hCreateUser pureAuthorization name login password flagMakeNews flagAdmin
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
        lUser <- stateExe stateAuth $ do 
          SAuthorization.hCreateUser pureAuthorization name login password flagMakeNews flagAdmin
          SAuthorization.hUserList pureAuthorization 0 3
        lUser `shouldBe` [ UserPublic
                         { nameUser = name,
                           loginUser = login,
                           dateCreationUser = day,
                           adminUser = flagAdmin,,
                           makeNewsUser = flagMakeNews
                         } ]
      it "hCheckAccount" $ do
        let name = "name1"
        let login = "login1"
        let password = "password1"
        let flagMakeNews = False
        let flagAdmin = False
        checkUser <- stateExe stateAuth $ do
          SAuthorization.hCreateUser pureAuthorization name login password flagMakeNews flagAdmin
          SAuthorization.hCheckAccount pureAuthorization login password
        checkUser `shouldBe` ( UserPublic
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
        checkUser <- stateExe stateAuth $ do
          SAuthorization.hCreateUser pureAuthorization name login password flagMakeNews flagAdmin
          SAuthorization.hGetAccount pureAuthorization login
        checkUser `shouldBe` ( Just $ UserPublic
                           { nameUser = name,
                             loginUser = login,
                             dateCreationUser = day,
                             adminUser = flagAdmin,
                             makeNewsUser = flagMakeNews
                           }
                       )


stateAuth = DataAuthorization empty

stateExe st s = exeState s st
