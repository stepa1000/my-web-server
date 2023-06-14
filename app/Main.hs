module Main (main) where

import Control.Monad
import Data.Config
import Data.Imp.Migration
import Data.Imp.Server
import Database.Beam.Postgres

main :: IO ()
main = do
  _ <- migrationDBServerMain
  conf <- getServerSettings
  server actExit (fromIntegral $ connectPort $ confConnectionInfo conf) conf

actExit :: IO ()
actExit = do
  putStrLn "print exit for exit"
  s <- getLine
  unless (s == "exit") actExit
