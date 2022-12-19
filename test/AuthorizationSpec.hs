{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module AuthorizationSpec (spec) where

import Prelude as P

import GHC.Generics

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Conduit as BPC
import Conduit

import Control.Applicative

import Data.Text
import Data.ByteString
import Data.Binary
import Data.ByteArray

import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Crypto.Hash
import Crypto.Hash.IO

import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldNotBe, shouldSatisfy)
import Test.QuickCheck (NonNegative (..), property, (==>))

import Data.News
import Data.User
import Data.Types

import qualified Control.Server.Authorization as SAuthorization
import qualified Data.Imp.Server.Authorization as ImpSAuthorization

spec :: Spec
spec = 
  describe ""

testDBConnect = defaultConnectInfo {connectUser="test", connectDatabase = "testDB"}
