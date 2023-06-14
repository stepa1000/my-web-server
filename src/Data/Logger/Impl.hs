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
withPreConf preConfig g = do
  withPreConf' preConfig (`withHandle` g)

withPreConf' :: PreConfig -> (Config -> IO a) -> IO a
withPreConf' preConfig g = do
  hendlerFile <- SIO.openFile (preconfFilePath preConfig) SIO.WriteMode
  a <-
    g $
      Config
        { confFileHandle = hendlerFile,
          confMinLevel = preconfMinLevel preConfig
        }
  SIO.hClose hendlerFile
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
liftHandleBaseIO logger = Logger.Handle {Logger.hLowLevelLog = \level text -> liftBase $ Logger.hLowLevelLog logger level text}

withHandle :: Config -> (Logger.Handle IO -> IO a) -> IO a
withHandle config f = do
  a <- f Logger.Handle {Logger.hLowLevelLog = logWith config}
  SIO.hClose $ confFileHandle config
  return a

logWith :: Config -> Logger.Level -> T.Text -> IO ()
logWith config logLvl text | logLvl >= confMinLevel config = do
  T.hPutStrLn (confFileHandle config) $ T.pack (show logLvl ++ ": ") `T.append` text
  SIO.hFlush (confFileHandle config)
logWith _ _ _ = return ()
