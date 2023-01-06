{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module NewsSpec (spec) where

import Prelude as P

import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC

import Data.Time.Clock
import Data.Maybe
import Data.Text
import Data.Vector as V

import Test.Hspec 
  ({-Expectation,-} Spec, around, describe, it, shouldBe {-, shouldNotBe, shouldSatisfy-})
--import Test.QuickCheck (NonNegative (..), property, (==>))

import Data.News
import Data.User
import Data.Types

import Control.Server.News as SN
import Data.Imp.Server.News as ISN

import qualified Control.Server.Authorization as SAuthorization
import qualified Data.Imp.Server.Authorization as ImpSAuthorization

-- spec = 

loginTest :: Login
loginTest = "loginTest"

nameTest :: Name
nameTest = "nameTest"

passwordTest :: Password
passwordTest = "testPassword"

newsCreateN :: Int -> [NewsCreate]
newsCreateN i = mapM f [0..i]
  where
    f j = NewsCreate 
      { nameNewsCreate = pack $ "nameNews" P.++ (show j)
      , categoryNewsCreate = pack $ "General"
      , textNewsCreate = pack $ "textNews" P.++ (show j)
      , photoNewsCreate = V.empty
      , newPhotoNewsCreate = V.empty
      , pulicNewsCreate = False
      }
