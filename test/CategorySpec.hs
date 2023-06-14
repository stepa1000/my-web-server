{-# LANGUAGE OverloadedStrings #-}

module CategorySpec (spec) where

import Control.Server.Category
import Data.Maybe
import Data.Tree
import Data.Types
import Imp.Pure.Category
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
  describe "category for news" $ do
    it "hGetCategory" $ do
      let generalCategory = stateExe stateCategory $ hGetCategory pureCategory
      generalCategory `shouldBe` Node "General" [Node "test" []]
    it "hCreateCategoryCreate" $ do
      testCategoryCreate `shouldBe` Node "General" [Node "test2" [], Node "test" []]
    it "hChangeCategoryChange" $ do
      testCategory `shouldBe` Node "General" [Node "test1" []]
  where
    testCategoryCreate = stateExe stateCategory $ do
      hCreateCategory pureCategory "General" "test2"
      hGetCategory pureCategory
    testCategory = stateExe stateCategory $ do
      hChangeCategory pureCategory "test" (Just "General") (Just "test1")
      hGetCategory pureCategory

stateCategory :: Tree Category
stateCategory = Node "General" [Node "test" []]
