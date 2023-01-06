module Main (main) where

-- import Lib

-- import System.IO as SIO

import Data.Imp.Server
import Data.Config

main :: IO ()
main = do -- return () -- someFunc
  conf <- getServerSettings
  server actExit conf

actExit :: IO ()
actExit = do
  putStrLn "print exit for exit"
  s <- getLine
  if s == "exit"
    then return ()
    else actExit
