module Main (main) where

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
  if s == "exit"
    then return ()
    else actExit
