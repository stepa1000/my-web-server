{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerSpec (spec) where

import Prelude as P

import Servant.API

--import Database.Beam
--import Database.Beam.Postgres as Beam
--import Database.Beam.Postgres.Conduit as BPC

import Servant.Client
-- import Servant.Client.Core.BaseUrl
-- import Network.HTTP.Client.TLS
import Network.HTTP.Client

import Data.Time.Clock
--import Data.Maybe
--import Data.Text
-- import qualified Data.ByteString as B
--import Data.Vector as V
import Data.IORef

import Test.Hspec 
  ({-Expectation,-} Spec, around, describe, it, shouldBe {-, shouldNotBe, shouldSatisfy-})
--import Test.QuickCheck (NonNegative (..), property, (==>))

--import Data.News
import Data.User
--import Data.Types

import Control.Concurrent

import API.Server.Web
--import Control.Server.News as SN
--import Data.Imp.Server.News as ISN

--import qualified Control.Server.Authorization as SAuthorization
--import qualified Data.Imp.Server.Authorization as ImpSAuthorization

import qualified Data.Config as Config
import qualified Data.Imp.Server as IS

import Data.Types

import Utils.News

spec :: Spec
spec = around withServer $ -- undefined
         describe "test for servant server" $ do
           it "server is life" $ \ce-> do
             l <- runClientM (do
               userList Nothing Nothing-- (Just 1) -- (Just 1) (Just 1)
               ) ce
             (UTCTime d _) <- getCurrentTime
             l `shouldBe` (Right [UserPublic 
               { nameUser = "tempAdmin"
               , loginUser = "tempAdmin"
               , dateCreationUser = d
               , adminUser = True
               , makeNewsUser = False
               }])   
           it "user add" $ \ce-> do
             ul <- runClientM (do 
               a <- userCreate
                 basicAuthDataTestTemp
                 (Just nameTest)
                 (Just loginTest)
                 (Just passwordTest)
                 (Just True)
                 (Just True)
               b <- userList Nothing Nothing
               return (a,b) 
               ) ce
             (UTCTime d _) <- getCurrentTime
             case ul of
               (Right (u,l)) -> l `shouldBe` [u, UserPublic 
                 { nameUser = "tempAdmin"
                 , loginUser = "tempAdmin"
                 , dateCreationUser = d
                 , adminUser = True
                 , makeNewsUser = False
                 }]
               (Left e) -> error $ show e
           it "server news create" $ \ce-> do
             e <- runClientM (do 
               ln <- mapM (createNewsNew basicAuthDataTest) (newsCreateN 5)
               lns <- getNewsPrivate 
                 basicAuthDataTest 
                 Nothing 
                 Nothing 
                 Nothing 
                 Nothing 
                 Nothing 
                 Nothing 
                 Nothing 
                 Nothing 
                 (Just False) 
                 Nothing 
                 Nothing 
                 Nothing 
               return (ln,lns)
               ) ce
             case e of
               (Right (ln,lns)) -> lns `shouldBe` ln
               (Left er) -> error $ show er

basicAuthDataTestTemp :: BasicAuthData
basicAuthDataTestTemp = (loginedToBasicAuthData $ Logined 
  { loginLogined = "tempAdmin"
  , passwordLogined = "temp"
  })

basicAuthDataTest :: BasicAuthData
basicAuthDataTest = (loginedToBasicAuthData $ Logined 
  { loginLogined = loginTest
  , passwordLogined = passwordTest
  })

withServer :: (ClientEnv -> IO ()) -> IO ()
withServer g = do
  refb <- newIORef False
  c <- Config.getServerSettingsTest
  _ <- forkIO $ IS.serverTest (exitAction refb) c (\(w,con)-> do
    w
    delateNews con
    deleteUser con
    )
  m <- newManager defaultManagerSettings -- tlsManagerSettings
  url <- (\burl-> burl {baseUrlPort = 3000} ) <$> parseBaseUrl "http://localhost"-- "https://127.0.0.1"-- "https://localhost"
  let ce = mkClientEnv m url
  Config.initNewsCategoryTest
  g ce
  writeIORef refb True
  where
    exitAction :: IORef Bool -> IO ()
    exitAction r = do
      b <- readIORef r
      if b then return ()
      else do
       threadDelay 10000
       exitAction r
  
