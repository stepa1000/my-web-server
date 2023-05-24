-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
{-# OPTIONS_GHC -Wwarn #-}

-- module Spec where

import Test.Hspec

import qualified NewsSpec
import qualified AuthorizationSpec
import qualified ServerSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  AuthorizationSpec.spec >>= 
  (const NewsSpec.spec) >>=
  (const ServerSpec.spec)
