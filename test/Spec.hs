-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
{-# OPTIONS_GHC -Wwarn #-}

import qualified AuthorizationSpec
import Data.Imp.Migration
import qualified NewsSpec
import qualified ServerSpec
import Test.Hspec

main :: IO ()
main = do
  _ <- migrationDBServerTest
  hspec spec

spec :: Spec
spec =
  AuthorizationSpec.spec
    >>= (const NewsSpec.spec)
    >>= (const ServerSpec.spec)
