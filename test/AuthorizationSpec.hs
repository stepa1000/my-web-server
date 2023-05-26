{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module AuthorizationSpec (spec) where

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
    around,
    describe,
    it,
    shouldBe,
  )
import Prelude as P

spec :: Spec
spec =
  around withDatabase $
    describe "server authorization" $ do
      it "hCreateUser" $ \(h, c) -> do
        let name = "name1"
        let login = "login1"
        let password = "password1"
        let fmn = False
        let fa = False
        up <- SAuthorization.hCreateUser h name login password fmn fa
        _ <-
          BPC.runDelete c $
            delete
              (ImpSAuthorization.dbUser ImpSAuthorization.webServerDB)
              (\a -> ImpSAuthorization._userLogin a ==. "login1")
        (UTCTime day _) <- getCurrentTime
        up
          `shouldBe` ( UserPublic
                         { nameUser = name,
                           loginUser = login,
                           dateCreationUser = day,
                           adminUser = fa,
                           makeNewsUser = fmn
                         }
                     )
      it "hUserList" $ \(h, c) -> do
        let name = "name1"
        let login = "login1"
        let password = "password1"
        let fmn = False
        let fa = False
        _ <- SAuthorization.hCreateUser h name login password fmn fa
        lup <- SAuthorization.hUserList h 0 3
        _ <-
          BPC.runDelete c $
            delete
              (ImpSAuthorization.dbUser ImpSAuthorization.webServerDB)
              (\a -> ImpSAuthorization._userLogin a ==. "login1")
        (UTCTime day _) <- getCurrentTime
        lup
          `shouldBe` [ UserPublic
                         { nameUser = name,
                           loginUser = login,
                           dateCreationUser = day,
                           adminUser = fa,
                           makeNewsUser = fmn
                         }
                     ]
      it "hCheckAccount" $ \(h, c) -> do
        let name = "name1"
        let login = "login1"
        let password = "password1"
        let fmn = False
        let fa = False
        _ <- SAuthorization.hCreateUser h name login password fmn fa
        up <- fromJust <$> SAuthorization.hCheckAccount h login password
        _ <-
          BPC.runDelete c $
            delete
              (ImpSAuthorization.dbUser ImpSAuthorization.webServerDB)
              (\a -> ImpSAuthorization._userLogin a ==. "login1")
        (UTCTime day _) <- getCurrentTime
        up
          `shouldBe` ( UserPublic
                         { nameUser = name,
                           loginUser = login,
                           dateCreationUser = day,
                           adminUser = fa,
                           makeNewsUser = fmn
                         }
                     )
      it "hGetAccount" $ \(h, c) -> do
        let name = "name1"
        let login = "login1"
        let password = "password1"
        let fmn = False
        let fa = False
        _ <- SAuthorization.hCreateUser h name login password fmn fa
        up <- fromJust <$> SAuthorization.hGetAccount h login
        _ <-
          BPC.runDelete c $
            delete
              (ImpSAuthorization.dbUser ImpSAuthorization.webServerDB)
              (\a -> ImpSAuthorization._userLogin a ==. "login1")
        (UTCTime day _) <- getCurrentTime
        up
          `shouldBe` ( UserPublic
                         { nameUser = name,
                           loginUser = login,
                           dateCreationUser = day,
                           adminUser = fa,
                           makeNewsUser = fmn
                         }
                     )

withDatabase :: ((SAuthorization.Handle IO, Connection) -> IO ()) -> IO ()
withDatabase act = do
  serverConfig <- getServerSettingsTest
  c <- connect $ confConnectionInfo serverConfig
  withPreConf (confLogger serverConfig) $ \logger -> do
    let h = ImpSAuthorization.makeHandle logger configAuthorization c
    act (h, c)
    _ <-
      BPC.runDelete c $
        delete
          (ImpSAuthorization.dbUser ImpSAuthorization.webServerDB)
          (\_ -> val_ True)
    Beam.close c

configAuthorization :: ImpSAuthorization.Config
configAuthorization =
  ImpSAuthorization.Config
    { ImpSAuthorization.confLimit = 3
    }
