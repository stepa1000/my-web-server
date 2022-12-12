{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The default implementation of the Logger interface.
module Logger.Impl
  ( withHandle,
    Config (..),
    liftHandleBaseIO
  )
where

import Control.Monad.Base

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Logger
import qualified System.IO as SIO

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

liftHandleBaseIO :: MonadBase IO m
                 => Logger.Handle IO -> Logger.Handle m
liftHandleBaseIO h = Logger.Handle {Logger.hLowLevelLog = \l t -> liftBase $ (Logger.hLowLevelLog h) l t } 

withHandle :: Config -> (Logger.Handle IO -> IO ()) -> IO ()
withHandle config f = do
  f Logger.Handle {Logger.hLowLevelLog = logWith config}
  SIO.hClose $ confFileHandle config

logWith :: Config -> Logger.Level -> T.Text -> IO ()
logWith conf logLvl t | logLvl >= (confMinLevel conf) = do
  T.hPutStrLn (confFileHandle conf) $ (T.pack $ show logLvl ++ ": " ) `T.append` t
  SIO.hFlush (confFileHandle conf)
logWith _ _ _ = return ()

