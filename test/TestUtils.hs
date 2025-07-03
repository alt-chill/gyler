module TestUtils where

import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Random (randomRIO)

import Control.Monad (replicateM)

randomString :: Int -> IO String
randomString len = replicateM len randomChar
  where
    chars = ['a'..'z']
    randomChar = do
        idx <- randomRIO (0, length chars - 1)
        return (chars !! idx)

withTempFilePath :: (FilePath -> IO a) -> IO a
withTempFilePath action =
    withSystemTempDirectory "temp-dir" $ \dir -> do
        -- Gen unique name
        str <- randomString 10
        let tempPath = dir </> ("ghost-" ++ str ++ ".txt")
        action tempPath
