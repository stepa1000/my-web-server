{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerSpec (spec) where

import Prelude as P

import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC

--import Data.Time.Clock
--import Data.Maybe
import Data.Text
-- import qualified Data.ByteString as B
import Data.Vector as V

import Test.Hspec 
  ({-Expectation,-} Spec, around, describe, it, shouldBe {-, shouldNotBe, shouldSatisfy-})
--import Test.QuickCheck (NonNegative (..), property, (==>))

import Data.News
--import Data.User
import Data.Types

import Control.Server.News as SN
import Data.Imp.Server.News as ISN

import qualified Control.Server.Authorization as SAuthorization
import qualified Data.Imp.Server.Authorization as ImpSAuthorization

import Utils.News

spec = undefined
