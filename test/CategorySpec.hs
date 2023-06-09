{-# LANGUAGE OverloadedStrings #-}

module CategorySpec (spec) where

import Control.Monad.State.Lazy
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
      generalCategory <- return $ stateExe stateCategory $ hGetCategory pureCategory
      generalCategory `shouldBe` (Node "General" [Node "test" []])
    it "hCreateCategory" $ do
      testCategory <- return $ stateExe stateCategory $ do
        hCreateCategory pureCategory "General" "test2"
        hGetCategory pureCategory
      testCategory `shouldBe` (Node "General" [Node "test2" [], Node "test" []])
    it "hChangeCategory" $ do
      testCategory <- return $ stateExe stateCategory $ do
        hChangeCategory pureCategory "test" (Just "General") (Just "test1")
        hGetCategory pureCategory
      testCategory `shouldBe` (Node "General" [Node "test1" []])

stateCategory :: Tree Category
stateCategory = Node "General" [Node "test" []]
