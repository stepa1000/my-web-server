{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module NewsSpec (spec) where

import Control.Server.News as SN
import Data.Imp.Database
import Data.Imp.Server.News as ISN
import Data.News
import Data.Text
import Data.Types
import Database.Beam
import Database.Beam.Postgres.Conduit as BPC
import Test.Hspec
import Utils.News
import Prelude as P

spec :: Spec
spec =
  around withDatabase $
    describe "test for database news" $ do
      it "create News" $ \(h, c) -> do
        n <- handleCreateNews h loginTest nameTest newsCreateTest
        l <- handleFind h Nothing emptySearch
        _ <-
          BPC.runDelete c $
            delete
              (dbNews webServerDB)
              (\a -> ISN._newsNewsName a ==. (val_ $ nameNews n))
        l `shouldBe` [n]
      {-      it "debug simple search news" $ \(h,c)-> do
              n <- handleCreateNews h loginTest nameTest newsCreateTest
              l <- hSearchNewsName 3 c "nameNews"
              _ <- BPC.runDelete c $ delete (ISN._news ISN.newsDB)
                     (\a-> ISN._newsNewsName a ==. (val_ $ nameNews n))
              l `shouldBe` [n]
      -}
      it "simple search news full" $ \(h, c) -> do
        n <- handleCreateNews h loginTest nameTest newsCreateTest
        l <- handleFind h Nothing $ idSearch n
        _ <-
          BPC.runDelete c $
            delete
              (dbNews webServerDB)
              (\a -> ISN._newsNewsName a ==. (val_ $ nameNews n))
        l `shouldBe` [n]
      it "simple search news" $ \(h, c) -> do
        n <- handleCreateNews h loginTest nameTest newsCreateTest
        l <- handleFind h Nothing $ emptySearch {mNewsName = Just "nameNews"}
        _ <-
          BPC.runDelete c $
            delete
              (dbNews webServerDB)
              (\a -> ISN._newsNewsName a ==. (val_ $ nameNews n))
        l `shouldBe` [n]
      it "simple search news date" $ \(h, c) -> do
        n <- handleCreateNews h loginTest nameTest newsCreateTest
        l <- handleFind h Nothing $ dateSearch n
        _ <-
          BPC.runDelete c $
            delete
              (dbNews webServerDB)
              (\a -> ISN._newsNewsName a ==. (val_ $ nameNews n))
        l `shouldBe` [n]
      it "simple search news published" $ \(h, c) -> do
        n <- handleCreateNews h loginTest nameTest newsCreateTest
        l <- handleFind h Nothing $ publicSearch n
        _ <-
          BPC.runDelete c $
            delete
              (dbNews webServerDB)
              (\a -> ISN._newsNewsName a ==. (val_ $ nameNews n))
        l `shouldBe` [n]
      it "simple search news content" $ \(h, c) -> do
        n <- handleCreateNews h loginTest nameTest newsCreateTest
        l <- handleFind h Nothing $ contentSearch n
        _ <-
          BPC.runDelete c $
            delete
              (dbNews webServerDB)
              (\a -> ISN._newsNewsName a ==. (val_ $ nameNews n))
        l `shouldBe` [n]
      it "debug simple search news content" $ \(h, c) -> do
        n <- handleCreateNews h loginTest nameTest newsCreateTest
        l <- hSearchContent 3 c (pack $ "textNews")
        _ <-
          BPC.runDelete c $
            delete
              (dbNews webServerDB)
              (\a -> ISN._newsNewsName a ==. (val_ $ nameNews n))
        l `shouldBe` [n]
      {-      it "debug position" $ \(h,c)-> do
              n <- handleCreateNews h loginTest nameTest newsCreateTest
              b <- debugPosition c (pack $ "textNews") -- handleFind h $ contentSearch n
              _ <- BPC.runDelete c $ delete (ISN._news ISN.newsDB)
                     (\a-> ISN._newsNewsName a ==. (val_ $ nameNews n))
              b `shouldBe` B.empty
      -}
      it "search news" $ \(h, c) -> do
        _ <- handleCreateNewsTestN h 10
        l <- handleFind h Nothing $ emptySearch {mNewsName = Just "nameNews5"} -- {mForString = Just "nameNews"}
        delateNews c
        (fmap textNews l) `shouldBe` ["textNews5"]
