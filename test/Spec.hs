{-# OPTIONS_GHC -Wwarn #-}

import qualified AuthorizationSpec
import qualified NewsSpec
import qualified ServerSpec
import Test.Hspec

main :: IO ()
main = do
  r <- migrationTest
  putStrLn $ show r
  hspec spec

spec :: Spec
spec =
  AuthorizationSpec.spec
    >>= (const NewsSpec.spec)
    >>= (const ServerSpec.spec)
