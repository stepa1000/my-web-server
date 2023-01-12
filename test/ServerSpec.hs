{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerSpec (spec) where

import Prelude as P

--import Database.Beam
--import Database.Beam.Postgres as Beam
--import Database.Beam.Postgres.Conduit as BPC

import Servant.Client
-- import Servant.Client.Core.BaseUrl
-- import Network.HTTP.Client.TLS
import Network.HTTP.Client

--import Data.Time.Clock
--import Data.Maybe
--import Data.Text
-- import qualified Data.ByteString as B
--import Data.Vector as V
import Data.IORef

import Test.Hspec 
  ({-Expectation,-} Spec, around, describe, it, shouldBe {-, shouldNotBe, shouldSatisfy-})
--import Test.QuickCheck (NonNegative (..), property, (==>))

--import Data.News
--import Data.User
--import Data.Types

import Control.Concurrent

import API.Server.Web
--import Control.Server.News as SN
--import Data.Imp.Server.News as ISN

--import qualified Control.Server.Authorization as SAuthorization
--import qualified Data.Imp.Server.Authorization as ImpSAuthorization

import qualified Data.Config as Config
import qualified Data.Imp.Server as IS

-- import Utils.News

spec :: Spec
spec = around withServer $ -- undefined
         describe "test for servant server" $ do
           it "server is life" $ \ce-> do
             l <- runClientM (do
               userList (Just 1) (Just 1)
               ) ce
             l `shouldBe` (Right [])     

withServer :: (ClientEnv -> IO ()) -> IO ()
withServer g = do
  refb <- newIORef False
  c <- Config.getServerSettingsTest
  _ <- forkIO $ IS.server (exitAction refb) c
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
  
