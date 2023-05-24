{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The default implementation of the Logger interface.
module Data.Logger.Impl
  ( withHandle,
    Config (..),
    liftHandleBaseIO,
    withPreConf,
    PreConfig (..),
  )
where

import qualified Control.Logger as Logger
import Control.Monad.Base
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Yaml
import GHC.Generics
import qualified System.IO as SIO

data PreConfig = PreConfig
  { preconfFilePath :: String,
    preconfMinLevel :: Logger.Level
  }
  deriving (Generic, FromJSON, ToJSON)

withPreConf :: PreConfig -> (Logger.Handle IO -> IO a) -> IO a
withPreConf pc g = do
  withPreConf' pc (`withHandle` g)

withPreConf' :: PreConfig -> (Config -> IO a) -> IO a
withPreConf' pc g = do
  h <- SIO.openFile (preconfFilePath pc) SIO.WriteMode
  a <-
    g $
      Config
        { confFileHandle = h,
          confMinLevel = preconfMinLevel pc
        }
  SIO.hClose h
  return a

data Config = Config
  { -- | A file handle to output formatted log messages to with
    -- 'System.IO.hPutStrLn' or 'Data.Text.IO.hPutStrLn'. For example,
    -- it might be 'System.IO.stderr' or a handle of a regular open
    -- file.
    confFileHandle :: SIO.Handle,
    -- | The minimum level of a printable log message. Messages with
    -- lower levels should not be printed.
    confMinLevel :: Logger.Level
  }

liftHandleBaseIO ::
  MonadBase IO m =>
  Logger.Handle IO ->
  Logger.Handle m
liftHandleBaseIO h = Logger.Handle {Logger.hLowLevelLog = \l t -> liftBase $ Logger.hLowLevelLog h l t}

withHandle :: Config -> (Logger.Handle IO -> IO a) -> IO a
withHandle config f = do
  a <- f Logger.Handle {Logger.hLowLevelLog = logWith config}
  SIO.hClose $ confFileHandle config
  return a

logWith :: Config -> Logger.Level -> T.Text -> IO ()
logWith conf logLvl t | logLvl >= confMinLevel conf = do
  T.hPutStrLn (confFileHandle conf) $ T.pack (show logLvl ++ ": ") `T.append` t
  SIO.hFlush (confFileHandle conf)
logWith _ _ _ = return ()
