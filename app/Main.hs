module Main (main) where

import Control.Monad
import Data.Config
import Data.Imp.Migration
import Data.Imp.Server

main :: IO ()
main = do
  _ <- migrationDBServerMain
  conf <- getServerSettings
  server actExit 5432 conf

actExit :: IO ()
actExit = do
  putStrLn "print exit for exit"
  s <- getLine
  unless (s == "exit") actExit
