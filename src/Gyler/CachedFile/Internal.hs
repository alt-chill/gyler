{-# LANGUAGE OverloadedStrings #-}

module Gyler.CachedFile.Internal (
     CachedFile (..)
    ,ReadFrom (..)
    ,newFile
    ,newFileDefault
    ,readCached
    ,getCachedContent
    ,fileAge
    ,isFileFresh
    ,readContent
    ,writeValue
    ,fetchOrRun
) where

import Data.IORef (IORef, writeIORef, newIORef, readIORef)
import Data.Time.Clock  (NominalDiffTime, diffUTCTime, getCurrentTime)

import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.IO as T (hGetContents, writeFile, readFile)

import System.Directory (getModificationTime)
import Control.Exception (try, catch, IOException, evaluate, SomeException (..))

import System.Process.Typed (proc, readProcessStdout_)

import qualified Data.Text.Encoding as T (decodeUtf8')
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString, toStrict)

import Gyler.Types

-- | Represents a cached file with:
--   - filePath: the path to the file on disk
--   - cache: an in-memory reference to the current content (if any)
--   - maxStaleAge: duration after which the file is considered stale
data CachedFile = CachedFile {
    filePath :: !FilePath,
    cache :: !(IORef (Maybe T.Text)), -- In-memory snapshot to reduce I/O
    maxStaleAge :: !NominalDiffTime   -- in seconds
}

-- | Describes the origin of the data returned by 'fetchOrRun'.
--   - Cache: from in-memory or file cache
--   - Executable: from running an external command
--   - Error: command failed or output was invalid
data ReadFrom = Cache | Executable | Error
    deriving (Show, Eq)

-- | Creates a new cached file with a custom staleness threshold.
newFile :: FilePath -> NominalDiffTime -> IO CachedFile
newFile path ttl = do
    ref <- newIORef Nothing
    return CachedFile { filePath = path, cache = ref, maxStaleAge = ttl }

-- | Creates a new cached file with a default staleness threshold (120 seconds).
newFileDefault :: FilePath -> IO CachedFile
newFileDefault file = newFile file defaultTime
    where defaultTime = 120

-- | Returns the current value stored in the in-memory cache
getCachedContent :: CachedFile -> IO (Maybe T.Text)
getCachedContent file = readIORef (cache file)

-- | Computes the age of the underlying file relative to the current time.
-- Returns the number of seconds since the file was last modified.
fileAge :: CachedFile -> IO NominalDiffTime
fileAge file = do
    currentTime <- getCurrentTime
    fileTime <- getModificationTime (filePath file)

    return (diffUTCTime currentTime fileTime)

-- Checks whether the cached file is still "fresh" by comparing
-- the modification age of the file to the allowed threshold.
-- Treat missing or unreadable file as not okay diff
isFileFresh :: CachedFile -> IO Bool
isFileFresh file = do
    tryDiff <- try (fileAge file) :: IO (Either IOError NominalDiffTime)
    case tryDiff of
        Right currentDiff -> return (currentDiff < maxStaleAge file)
        Left _ -> return False

-- | Attempts to read the file from disk and update the in-memory cache.
-- If reading fails (e.g. due to missing or unreadable file), returns Nothing
-- and leaves the cache unchanged.
readContent :: CachedFile -> IO (Maybe T.Text)
readContent file  = do
    let handler :: IOException -> IO (Maybe T.Text)
        handler _ = return Nothing

    content <- fmap Just (T.readFile (filePath file)) `catch` handler

    case content of
        Just _  -> writeIORef (cache file) content
        Nothing -> pure ()

    return content


-- | Retrieves the cached value.
-- If no value is cached:
--   - Tries to read from disk if the file is still fresh.
--   - Returns Nothing if the file is stale or unreadable.
readCached :: CachedFile -> IO (Maybe T.Text)
readCached file = do
    currentValue <- getCachedContent file

    case currentValue of
        Nothing  -> do diffOk <- isFileFresh file
                       if diffOk
                           then readContent file    -- load from disk
                           else return Nothing      -- nothing in cache
        v@(Just val) -> return v                    -- cached value

-- | Writes the given value both to the file on disk and to the in-memory cache.
-- If the file write fails, the cache is still updated in memory.
writeValue :: CachedFile -> T.Text -> IO ()
writeValue file value = do
    let handler :: IOException -> IO ()
        handler _ = return ()

    writeIORef (cache file) (Just value)
    T.writeFile (filePath file) value `catch` handler

-- | Retrieves a value from cache or runs an external command to produce it.
--
-- Behavior:
--   - If a valid cached value exists, it is returned immediately.
--   - If not, the external command is executed.
--     - On success: the output is saved to file and cache, and returned.
--     - On failure (e.g. command fails or output can't be decoded): returns an empty string.
--
-- The first value in pair indicates the source of the data: Cache, Executable, or Error.
fetchOrRun :: CachedFile -> Cmd -> IO (ReadFrom, T.Text)
fetchOrRun file (exec, args) = do
    cached <- readCached file
    case cached of
        Just val -> return (Cache, val)
        Nothing -> do
            result <- tryRunProcess
            case result of
                Just output -> do
                    writeValue file output
                    return (Executable, output)
                Nothing -> return (Error, "")
  where
    tryRunProcess :: IO (Maybe T.Text)
    tryRunProcess = do
        let process = proc (T.unpack exec) (map T.unpack args)
        result <- try (readProcessStdout_  process) :: IO (Either SomeException BL.ByteString)
        return $ case result of
            Left _ -> Nothing
            Right outBS ->
                case T.decodeUtf8' (BL.toStrict outBS) of
                    Right txt -> Just txt
                    Left _    -> Nothing
