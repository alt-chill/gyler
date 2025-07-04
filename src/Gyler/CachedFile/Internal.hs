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

data CachedFile = CachedFile {
    filePath :: !FilePath,
    cache :: !(IORef (Maybe T.Text)),
    maxStaleAge :: !NominalDiffTime -- in seconds
}

data ReadFrom = Cache | Executable | Error deriving (Show, Eq)

newFile :: FilePath-> NominalDiffTime -> IO CachedFile
newFile file time = do
    ref <- newIORef Nothing
    return CachedFile {
        filePath = file,
        cache = ref,
        maxStaleAge = time
    }

newFileDefault :: FilePath -> IO CachedFile
newFileDefault file = newFile file defaultTime
    where defaultTime = 120

getCachedContent :: CachedFile -> IO (Maybe T.Text)
getCachedContent file = readIORef (cache file)

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

-- Reads file content from disk and caches it.
-- Returns an Nothing if reading fails (e.g., file not found).
-- Do not clean the cache if reading fails
readContent :: CachedFile -> IO (Maybe T.Text)
readContent file  = do
    let handler :: IOException -> IO (Maybe T.Text)
        handler _ = return Nothing

    content <- fmap Just (T.readFile (filePath file)) `catch` handler

    case content of
        Just _  -> writeIORef (cache file) content
        Nothing -> pure ()

    return content

-- Main access point for cached value.
-- Uses cache if available, otherwise:
--   - if file is fresh, load from disk
--   - if file is stale, return Nothing
readCached :: CachedFile -> IO (Maybe T.Text)
readCached file = do
    currentValue <- getCachedContent file

    case currentValue of
        Nothing  -> do diffOk <- isFileFresh file
                       if diffOk
                           then readContent file    -- load from disk
                           else return Nothing      -- nothing in cache
        v@(Just val) -> return v                    -- cached value

-- Writes value to cache and into file
-- Doing nothing if write is failed
writeValue :: CachedFile -> T.Text -> IO ()
writeValue file value = do
    let handler :: IOException -> IO ()
        handler _ = return ()

    writeIORef (cache file) (Just value)
    T.writeFile (filePath file) value `catch` handler

-- | Attempts to retrieve a cached value.
-- If not available or stale, runs the external command and captures its stdout.
-- The result is saved in the file and returned.
-- If the command fails or decoding fails, returns an empty string.
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
