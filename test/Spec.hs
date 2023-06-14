-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
{-# OPTIONS_GHC -Wwarn #-}

import qualified AuthorizationSpec
import qualified CategorySpec
import qualified NewsSpec
import Test.Hspec

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  AuthorizationSpec.spec
  NewsSpec.spec
  CategorySpec.spec
