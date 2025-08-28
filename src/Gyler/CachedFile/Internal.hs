{-# LANGUAGE OverloadedStrings #-}

module Gyler.CachedFile.Internal (
     CachedFile (..)
    ,ReadFrom (..)
    ,newFile
    ,newFileDefault
    ,readData
    ,getCachedContent
    ,fileAge
    ,isFileFresh
    ,readFromDisk
    ,writeValue
    ,fetchOrRun
) where

import Data.IORef (IORef, writeIORef, newIORef, readIORef)
import Data.Time.Clock  (NominalDiffTime, diffUTCTime, getCurrentTime)

import qualified Data.Text as T (Text, pack, unpack)

import System.Directory (getModificationTime, removeFile)
import Control.Exception (try, catch, evaluate, SomeException (..))

import System.Process.Typed (proc, readProcessStdout_)

import qualified Data.Text.Encoding as T (decodeUtf8')
import qualified Data.ByteString as BS (ByteString, readFile, writeFile)
import qualified Data.ByteString.Lazy as BL (ByteString, toStrict)

import qualified Gyler.Data.NonEmptyText as NET (unpack)

import Gyler.Types

-- | Represents a cached file with:
--   - filePath: the path to the file on disk
--   - cache: an in-memory reference to the current content (if any)
--   - maxStaleAge: duration after which the file is considered stale
data CachedFile = CachedFile {
    filePath :: !FilePath,
    cache :: !(IORef (Maybe BS.ByteString)), -- In-memory snapshot to reduce I/O
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
getCachedContent :: CachedFile -> IO (Maybe BS.ByteString)
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
readFromDisk :: CachedFile -> IO (Maybe BS.ByteString)
readFromDisk file  = do
    let handler :: SomeException -> IO (Maybe BS.ByteString)
        handler _ = return Nothing

    content <- fmap Just (BS.readFile (filePath file)) `catch` handler

    case content of
        Just _  -> writeIORef (cache file) content
        Nothing -> pure ()

    return content


-- | Retrieves the cached value.
-- If no value is cached:
--   - Tries to read from disk if the file is still fresh.
--   - Returns Nothing if the file is stale or unreadable.
readData :: CachedFile -> IO (Maybe BS.ByteString)
readData file = do
    currentValue <- getCachedContent file

    case currentValue of
        Nothing  -> do diffOk <- isFileFresh file
                       if diffOk
                           then readFromDisk file    -- load from disk
                           else return Nothing      -- nothing in cache
        v@(Just val) -> return v                    -- cached value

-- | Writes the given value both to the file on disk and to the in-memory cache.
-- If the file write fails, the cache is still updated in memory.
writeValue :: CachedFile -> BS.ByteString -> IO ()
writeValue file value = do
    let handler :: SomeException -> IO ()
        handler _ = return ()

    writeIORef (cache file) (Just value)
    BS.writeFile (filePath file) value `catch` handler

-- | Clears the in-memory cache and attempts to remove file from disk.
--
-- This operation is unconditional: it does not check if the file exists
-- or if the process has write permissions. All errors are caught and ignored.
clearCache :: CachedFile -> IO ()
clearCache file = do
    let handler :: SomeException -> IO ()
        handler _ = return ()

    writeIORef (cache file) Nothing
    removeFile (filePath file) `catch` handler

-- | Retrieves a value from cache or runs an external command to produce it.
--
-- Behavior:
--   - If a valid cached value exists, it is returned immediately.
--   - If not, the external command is executed.
--     - On success: the output is saved to file and cache, and returned.
--     - On failure (e.g. command fails or output can't be decoded): returns an empty string.
--
-- The first value in pair indicates the source of the data: Cache, Executable, or Error.
fetchOrRun :: CachedFile -> Cmd -> IO (ReadFrom, BS.ByteString)
fetchOrRun file (exec, args) = do
    cached <- readData file
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
    tryRunProcess :: IO (Maybe BS.ByteString)
    tryRunProcess = do
        let process = proc (NET.unpack exec) (map NET.unpack args)
        result <- try (readProcessStdout_  process) :: IO (Either SomeException BL.ByteString)
        return $ case result of
            Right outBS -> Just . BL.toStrict $ outBS
            Left _      -> Nothing
