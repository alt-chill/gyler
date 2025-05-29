module Gyler.CachedFile.Internal (
    CachedFile (..)
    ,newFile
    ,newFileDefault
    ,getValue
    ,getCachedContent
    ,fileAge
    ,isFileFresh
    ,readContent
) where

import Data.IORef (IORef, writeIORef, newIORef, readIORef)
import Data.Time.Clock  (NominalDiffTime, diffUTCTime, getCurrentTime)

import qualified Data.Text as T (Text)
import qualified Data.Text.IO as T (hGetContents, writeFile, readFile)

import System.Directory (getModificationTime)
import Control.Exception (try, catch, IOException, evaluate)

data CachedFile = CachedFile {
    filePath :: !FilePath,
    cache :: !(IORef (Maybe T.Text)),
    maxStaleAge :: !NominalDiffTime -- in seconds
}

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
readContent :: CachedFile -> IO (Maybe T.Text)
readContent file  = do
    let handler :: IOException -> IO (Maybe T.Text)
        handler _ = return Nothing

    content <- fmap Just (T.readFile (filePath file)) `catch` handler
    writeIORef (cache file) content

    return content

-- Main access point for cached value.
-- Uses cache if available, otherwise:
--   - if file is fresh, load from disk
--   - if file is stale, return Nothing
getValue :: CachedFile -> IO (Maybe T.Text)
getValue file = do
    currentValue <- getCachedContent file

    case currentValue of
        Nothing  -> do diffOk <- isFileFresh file
                       if diffOk
                           then readContent file    -- load from disk
                           else return Nothing      -- nothing in cache
        v@(Just val) -> return v                    -- cached value
