{-# LANGUAGE OverloadedStrings #-}

module NewsSpec (spec) where

import Control.Monad.State.Lazy
import Control.Server.News
import Control.Server.Photo
import Data.Map as Map
import Data.Maybe
import Data.News
import Data.Text
import Data.Time.Calendar.OrdinalDate
import Data.Types
import Data.UUID
import Imp.Pure.News
import System.Random
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Utils
import Prelude as P

spec :: Spec
spec = do
  describe "News" $ do
    it "hSearchNews" $ do
      lNews <-
        return $
          stateExe statePhoto $
            stateExeT stateNews $
              hSearchNews (pureNews dayTest) Nothing (emptySearch {mNewsName = Just "testNews"})
      lNews `shouldBe` [testNews]
    it "hPutNews" $ do
      lNews <- return $ stateExe statePhoto $ stateExeT stateNews $ do
        hPutNews (pureNews dayTest) testNews3
        hSearchNews (pureNews dayTest) Nothing (emptySearch {mNewsName = Just "testNews3"})
      lNews `shouldBe` [testNews3]
    it "hGenUUID" $ do
      uuid <- return $ stateExe statePhoto $ stateExeT stateNews $ hGenUUID (pureNews dayTest)
      print uuid
    it "hGetNews" $ do
      mNews <- return $ stateExe statePhoto $ stateExeT stateNews $ hGetNews (pureNews dayTest) (uuidNews testNews)
      mNews `shouldBe` (Just testNews)
    it "hModifNews" $ do
      modNews <- return $ stateExe statePhoto $ stateExeT stateNews $ do
        hModifNews (pureNews dayTest) (uuidNews testNews) (\test1 -> test1 {textNews = "newContent"})
        hGetNews (pureNews dayTest) (uuidNews testNews)
      modNews `shouldBe` (Just (testNews {textNews = "newContent"}))
    it "hGetDay" $ do
      day <- return $ stateExe statePhoto $ stateExeT stateNews $ hGetDay (pureNews dayTest)
      day `shouldBe` dayTest
  describe "Photos" $ do
    it "hPutPhoto" $ do
      base64 <- return $ stateExe statePhoto $ stateExeT stateNews $ do
        uuidPhoto <- hPutPhoto (handlePhoto (pureNews dayTest)) "photoimg"
        hGetPhoto (handlePhoto (pureNews dayTest)) uuidPhoto
      base64 `shouldBe` (Just "photoimg")
    it "hGetPhoto" $ do
      base64 <- return $ stateExe statePhoto $ stateExeT stateNews $ hGetPhoto (handlePhoto (pureNews dayTest)) uuidtestPhoto
      base64 `shouldBe` (Just base64testPhoto)

dayTest :: Day
dayTest = fromMondayStartWeek 2023 42 7

stateNews :: Map UUID News
stateNews = Map.singleton (uuidNews testNews) testNews <> Map.singleton (uuidNews testNews2) testNews2

testNews, testNews2, testNews3 :: News
(testNews, testNews2, testNews3) = stateExe (fst statePhoto, mkStdGen 1) $ stateExeT stateNews $ do
  t1 <-
    handleCreateNewsTest
      (pureNews dayTest)
      ( exempleNewsCreate
          { nameNewsCreate = "testNews"
          }
      )
  t2 <-
    handleCreateNewsTest
      (pureNews dayTest)
      ( exempleNewsCreate
          { nameNewsCreate = "testNews2"
          }
      )
  t3 <-
    handleCreateNewsTest
      (pureNews dayTest)
      ( exempleNewsCreate
          { nameNewsCreate = "testNews3"
          }
      )
  return (t1, t2, t3)

uuidtestPhoto :: UUID
uuidtestPhoto = fromJust $ fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"

base64testPhoto :: Base64
base64testPhoto = pack "base64TestPhoto"

statePhoto :: (Map Photo Base64, StdGen)
statePhoto = (Map.singleton uuidtestPhoto base64testPhoto, mkStdGen 2)

handleCreateNewsTest :: Monad m => Control.Server.News.Handle m -> NewsCreate -> m News
handleCreateNewsTest h = handleCreateNews h loginTest nameTest

{-
newsCreateTest :: NewsCreate
newsCreateTest =
  NewsCreate
    { nameNewsCreate = pack $ "nameNews",
      categoryNewsCreate = pack $ "General",
      textNewsCreate = pack $ "textNews",
      photoNewsCreate = V.empty,
      newPhotoNewsCreate = V.empty,
      publicNewsCreate = False
    }
-}
loginTest :: Login
loginTest = "loginTest"

nameTest :: Name
nameTest = "nameTest"
