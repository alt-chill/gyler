{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Gyler.Logging
-- Description: Logging system based on fast-logger
--
-- The central component is 'LogFunc', which takes the severity and the text of
-- a message. All logging logic is encapsulated in this function. In Gyler, the
-- 'createLogger' function constructs an appropriate logger to be used within
-- the 'GylerM' monad.
--
-- This module also provides the 'HasLogger' typeclass for environments. It
-- allows the use of 'log[Severity]' functions inside 'MonadReader' contexts.
--
-- == Data types
--
-- The system supports four severity levels:
--
-- * 'Debug'   – detailed information useful for debugging
-- * 'Info'    – general operational messages
-- * 'Warning' – potential issues or recoverable problems
-- * 'Error'   – serious failures requiring attention
--
-- == Typeclasses
--
-- * 'HasLogger' – environments that provide a logging function.
--
--   Its definition is:
--
--   @
--   getLogger :: e -> LogFunc
--   @
--
-- == Key functions
--
-- * 'createLogger' – initializes a logger that writes messages to a file with
--                    severity greater than or equal to the given 'minSeverity'.
--
-- * 'logDebug', 'logInfo', 'logWarning', 'logError' – convenience functions
--   for logging messages within 'HasLogger' 'MonadReader' environments.
--
-- == Example
--
-- @
-- import Gyler.Logging
-- import Control.Monad.Reader
--
-- data Env = Env { logger :: LogFunc }
--
-- instance HasLogger Env where
--     getLogger = logger
--
-- main :: IO ()
-- main = do
--     res <- createLogger Info "app.log"
--     case res of
--         Left err -> putStrLn $ "Failed to initialize logger: " ++ show err
--         Right (logFunc, cleanUp) -> do
--             let env = Env logFunc
--             runReaderT (logInfo "Application started") env
--             cleanUp
-- @
--
-- See also: "Gyler.Context"

module Gyler.Logging (
    -- Data types and typeclasses
    Severity (..),
    HasLogger (..),

    -- Type synonyms
    LogFunc,
    CleanAction,

    -- Functions
    createLogger,
    logDebug,
    logInfo,
    logWarning,
    logError
) where

import Control.Monad.IO.Class (MonadIO (..))

import Control.Monad (when)

import Data.Text (Text)

import System.Log.FastLogger (TimedFastLogger, LogStr, toLogStr, LogType' (..),
                              newTimeCache, simpleTimeFormat', newTimedFastLogger, ToLogStr)

import Control.Exception (try, bracket, SomeException)
import Control.Monad.Reader (MonadReader, asks)

-- | A logging function that accepts a severity and a message.
type LogFunc = Severity -> Text -> IO ()

-- | An action that cleans up resources associated with the logger.
type CleanAction = IO ()

-- | Represents the severity level of a log message.
--
-- Debug < Info < Warning < Error
data Severity
    -- | Useful information for debugging.
    = Debug
    -- | General operational information.
    | Info
    -- | Warnings or non-critical failures.
    | Warning
    -- | Errors or severe failures.
    | Error
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance ToLogStr Severity where
    toLogStr = toLogStr . show

-- | Internal function that formats and writes log messages.
--
-- See: "System.Log.FastLogger"
useLogger :: MonadIO m => TimedFastLogger -> Severity -> Severity -> Text -> m ()
useLogger logger minSev sev msg = when (sev >= minSev) $
                liftIO $ logger (\time ->
                         mconcat [toLogStr time, " [", toLogStr sev, "] ", toLogStr msg, "\n"])

-- | Creates a logger that writes to the specified file with a given minimum severity level.
--
-- You must call the returned 'CleanAction' after you finish using the logger.
createLogger :: Severity -> FilePath -> IO (Either SomeException (LogFunc, CleanAction))
createLogger minSev file = try $ do
    let bufSize = 512
        logType = LogFileNoRotate file bufSize

    timer <- newTimeCache simpleTimeFormat'
    (logger, cleanUp) <- newTimedFastLogger timer logType

    return (useLogger logger minSev, cleanUp)

-- | A typeclass for environments that provide a logger.
class HasLogger e where
    getLogger :: e -> LogFunc

-- | Internal helper function for logging messages with a given severity.
logM :: (HasLogger e, MonadReader e m, MonadIO m) => Severity -> Text -> m ()
logM sev msg = do
    func <- asks getLogger
    liftIO $ func sev msg

-- | Logs a debug message.
logDebug   :: (HasLogger e, MonadReader e m, MonadIO m) => Text -> m ()
logDebug   = logM Debug

-- | Logs an informational message.
logInfo    :: (HasLogger e, MonadReader e m, MonadIO m) => Text -> m ()
logInfo    = logM Info

-- | Logs a warning message.
logWarning :: (HasLogger e, MonadReader e m, MonadIO m) => Text -> m ()
logWarning = logM Warning

-- | Logs an error message.
logError   :: (HasLogger e, MonadReader e m, MonadIO m) => Text -> m ()
logError   = logM Error
