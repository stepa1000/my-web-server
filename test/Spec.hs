-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
{-# OPTIONS_GHC -Wwarn #-}

import qualified AuthorizationSpec
import qualified CategorySpec
import Data.Imp.Migration
import qualified NewsSpec
import Test.Hspec

main :: IO ()
main = do
  _ <- migrationDBServerTest
  hspec spec

spec :: Spec
spec = do
  AuthorizationSpec.spec
  NewsSpec.spec
  CategorySpec.spec
