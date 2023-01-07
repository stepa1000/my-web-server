-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
{-# OPTIONS_GHC -Wwarn #-}

-- module Spec where

import Test.Hspec

import qualified NewsSpec
import qualified AuthorizationSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  AuthorizationSpec.spec >>= 
  (const NewsSpec.spec)
