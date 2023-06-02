{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ServerSpec (spec) where

import API.Server.Web
import Control.Concurrent
import Data.Config (getServerSettingsTest)
import Data.IORef
import qualified Data.Imp.Server as IS
import Data.News
import Data.Time.Clock
import Data.Tree
import Data.Types
import Data.User
import Network.HTTP.Client
import Servant.API
import Servant.Client
import Test.Hspec
import Utils.News
import Prelude as P

spec :: Spec
spec = aroundAll withServer $
  describe "test for servant server" $ do
    it "server is life" $ \ce -> do
      l <-
        runClientM
          ( do
              userList Nothing Nothing
          )
          ce
      (UTCTime d _) <- getCurrentTime
      l
        `shouldBe` ( Right
                       [ UserPublic
                           { nameUser = "tempAdmin",
                             loginUser = "tempAdmin",
                             dateCreationUser = d,
                             adminUser = True,
                             makeNewsUser = False
                           }
                       ]
                   )
    it "user add" $ \ce -> do
      ul <-
        runClientM
          ( do
              a <-
                userCreate
                  basicAuthDataTestTemp
                  (Just nameTest)
                  (Just loginTest)
                  (Just passwordTest)
                  (Just True)
                  (Just True)
              b <- userList Nothing Nothing
              return (a, b)
          )
          ce
      (UTCTime d _) <- getCurrentTime
      case ul of
        (Right (u, l)) ->
          l
            `shouldBe` [ u,
                         UserPublic
                           { nameUser = "tempAdmin",
                             loginUser = "tempAdmin",
                             dateCreationUser = d,
                             adminUser = True,
                             makeNewsUser = False
                           }
                       ]
        (Left e) -> error $ show e
    it "server news create" $ \ce -> do
      e <-
        runClientM
          ( do
              ln <- mapM (createNewsNew basicAuthDataTest) (newsCreateN 5)
              lns <-
                getNewsPrivate
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
              return (ln, lns)
          )
          ce
      case e of
        (Right (ln, lns)) -> lns `shouldBe` ln
        (Left er) -> error $ show er
    it "server search publik news" $ \ce -> do
      e <-
        runClientM
          ( do
              getNewsPublicAny
          )
          ce
      e `shouldBe` (Right [])
    it "server eddit news" $ \ce -> do
      e <-
        runClientM
          ( do
              luuID <-
                (fmap . fmap) uuidNews $
                  getNewsPrivate
                    basicAuthDataTest
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    (Just "nameNews3")
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
              case luuID of
                (uuID : _) -> do
                  n <-
                    createNewsEdit
                      basicAuthDataTest
                      (Just uuID)
                      Nothing
                      Nothing
                      Nothing
                      (Just True)
                      []
                      []
                  l <- getNewsPublicAny
                  return (l, n)
                _ -> error "test error: news not found"
          )
          ce
      case e of
        (Right (l, n)) -> l `shouldBe` [n]
        (Left er) -> error $ show er
    it "get tree" $ \ce -> do
      e <-
        runClientM
          ( do
              categoryGetTree
          )
          ce
      e `shouldBe` (Right (Node "General" []))
    it "create category" $ \ce -> do
      e <-
        runClientM
          ( do
              categoryCreate
                basicAuthDataTest
                (Just "General")
                (Just "testCategory")
          )
          ce
      e `shouldBe` (Right (Node "General" [(Node "testCategory" [])]))
    it "change category" $ \ce -> do
      e <-
        runClientM
          ( do
              categoryChange
                basicAuthDataTest
                (Just "testCategory")
                Nothing
                (Just "testCategory2")
          )
          ce
      e `shouldBe` (Right (Node "General" [(Node "testCategory2" [])]))

getNewsPublicAny :: ClientM [News]
getNewsPublicAny =
  getNewsPublic
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

basicAuthDataTestTemp :: BasicAuthData
basicAuthDataTestTemp =
  ( loginedToBasicAuthData $
      Logined
        { loginLogined = "tempAdmin",
          passwordLogined = "temp"
        }
  )

basicAuthDataTest :: BasicAuthData
basicAuthDataTest =
  ( loginedToBasicAuthData $
      Logined
        { loginLogined = loginTest,
          passwordLogined = passwordTest
        }
  )

withServer :: (ClientEnv -> IO ()) -> IO ()
withServer g = do
  refb <- newIORef False
  serverConfig <- getServerSettingsTest
  thread <-
    forkIO $
      IS.serverTest
        (exitAction refb)
        3000
        serverConfig
        ( \(w, con) -> do
            w
            delateNews con
            deleteUser con
        )
  print thread
  m <- newManager defaultManagerSettings
  url <- (\burl -> burl {baseUrlPort = 3000}) <$> parseBaseUrl "http://127.0.0.1" -- "https://127.0.0.1"-- "https://localhost"
  let ce = mkClientEnv m url
  g ce
  writeIORef refb True
  where
    exitAction :: IORef Bool -> IO ()
    exitAction r = do
      b <- readIORef r
      if b
        then return ()
        else do
          threadDelay 10000
          exitAction r
