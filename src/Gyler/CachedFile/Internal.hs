module Gyler.CachedFile.Internal (
    CachedFile (..),
    newFile,
    newFileDefault,
    getValue,
    storedData,
    calcDiff,
    isDiffOk,
    updateFile,
    readContent
) where

import Data.IORef (IORef, writeIORef, newIORef, readIORef)
import Data.Time.Clock  (NominalDiffTime, diffUTCTime, getCurrentTime)

import System.Process (createProcess, CreateProcess (..) , proc, waitForProcess, StdStream(..))
import System.IO (hClose)

import qualified Data.ByteString.Lazy as BL (ByteString, hGetContents, writeFile, readFile, empty)
import qualified Data.ByteString.Lazy.Char8 as BLC ()

import System.Directory (getModificationTime)
import Control.Exception (try, catch, IOException, evaluate)

type Exec = String
type Args = [String]
type Command = (Exec, Args)

data CachedFile = CachedFile {
    filePath :: !FilePath,
    cache :: !(IORef (Maybe BL.ByteString)),
    availableDiff :: !NominalDiffTime,
    updateCommand :: !Command
}

newFile :: FilePath -> Command -> NominalDiffTime -> IO CachedFile
newFile file command time = do
    ref <- newIORef Nothing
    return CachedFile {
        filePath = file,
        cache = ref,
        availableDiff = time,
        updateCommand = command
    }

newFileDefault :: FilePath -> Command -> IO CachedFile
newFileDefault file command = newFile file command defaultTime
    where defaultTime = 120

storedData :: CachedFile -> IO (Maybe BL.ByteString)
storedData file = readIORef (cache file)

calcDiff :: CachedFile -> IO NominalDiffTime
calcDiff file = do
    currentTime <- getCurrentTime
    fileTime <- getModificationTime (filePath file)

    return (diffUTCTime currentTime fileTime)

-- Checks whether the cached file is still "fresh" by comparing
-- the modification age of the file to the allowed threshold.
-- Treat missing or unreadable file as not okay diff
isDiffOk :: CachedFile -> IO Bool
isDiffOk file = do
    tryDiff <- try (calcDiff file) :: IO (Either IOError NominalDiffTime)
    case tryDiff of
        Right currentDiff -> return (currentDiff < availableDiff file)
        Left _ -> return False

-- Runs the external command to regenerate the file content,
-- updates the cache and attempts to write the result to disk.
-- Tries to write output to disk for future reuse. Failure is ignored
updateFile :: CachedFile -> IO BL.ByteString
updateFile file = do
    (hIn, hOut, _, ph) <- createProcess (uncurry proc (updateCommand file))
                                        { std_in = CreatePipe, std_out = CreatePipe }

    case (hIn, hOut) of
        (Just hin, Just hout) -> do
            hClose hin

            result <- BL.hGetContents hout
            _ <- evaluate result   -- strictness
            _ <- waitForProcess ph

            writeIORef (cache file) (Just result)

            let handler :: IOException -> IO ()
                handler _ = return ()
            BL.writeFile (filePath file) result `catch` handler

            return result

        _Failure  -> error $ "Cannot open pipes for " ++ show (updateCommand file)

-- Reads file content from disk and caches it.
-- Returns an empty ByteString if reading fails (e.g., file not found).
readContent :: CachedFile -> IO BL.ByteString
readContent file  = do
    let handler :: IOException -> IO BL.ByteString
        handler _ = return BL.empty

    content <- BL.readFile (filePath file) `catch` handler
    writeIORef (cache file) (Just content)

    return content

-- Main access point for cached value.
-- Uses cache if available, otherwise:
--   - if file is fresh, load from disk
--   - if file is stale, regenerate it using the updateCommand
getValue :: CachedFile -> IO BL.ByteString
getValue file = do
    currentValue <- storedData file

    case currentValue of
        Nothing  -> do diffOk <- isDiffOk file
                       if diffOk
                           then readContent file    -- load from disk
                           else updateFile file     -- run command
        Just val -> return val                      -- cached value
