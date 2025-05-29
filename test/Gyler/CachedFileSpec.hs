{-# LANGUAGE OverloadedStrings #-}

module Gyler.CachedFileSpec (spec) where

import Gyler.CachedFile.Internal

import Test.Hspec
import System.IO.Temp (withSystemTempFile)
import qualified Data.Text    as T (Text)
import System.Directory (getModificationTime, removeFile)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, addUTCTime, diffUTCTime)
import System.Process (callProcess)
import System.IO (hClose, hPutStr, hPutStrLn)
import Data.IORef (writeIORef)

spec :: Spec
spec = describe "Gyler.CachedFile" $ do
    it "newFileDefault initializes with empty cache" $ do
        file <- newFileDefault "somefile"
        cached <- getCachedContent file
        cached `shouldBe` Nothing

    it "getValue returns Nothing for non-existing file" $ do
        file <- newFileDefault "somefile"
        value <- getValue file
        value `shouldBe` Nothing

    it "getValue returns Nothing for stale file" $ do
        withSystemTempFile "test.txt" $ \fp h -> do
            hPutStrLn h "Ignored"
            hClose h

            file <- newFile fp 0.001
            threadDelay 10000

            value <- getValue file
            value `shouldBe` Nothing

    it "getValue reads and caches content of file" $
        withSystemTempFile "test.txt" $ \fp h -> do
            hPutStr h "hello\n"
            hClose h

            file <- newFile fp 1000

            value  <- getValue file
            cached <- getCachedContent file

            value `shouldBe` Just "hello\n"
            cached `shouldBe` Just "hello\n"

    it "getValue always return cached value after reading" $
        withSystemTempFile "test.txt" $ \fp h -> do
            hPutStr h "hello\n"
            hClose h

            file <- newFileDefault fp

            value  <- getValue file
            cached <- getCachedContent file

            value `shouldBe` Just "hello\n"
            cached `shouldBe` Just "hello\n"

            writeFile fp "ignored"

            value2  <- getValue file
            value2 `shouldBe` value

    it "isFileFresh returns True for recently modified files" $ do
        withSystemTempFile "test.txt" $ \fp h -> do
            hPutStr h "recent"
            hClose h

            file <- newFile fp 1000
            ok <- isFileFresh file

            ok `shouldBe` True

    it "isFileFresh returns False for non-existent file" $ do
        let fakeFile = "nonexistent.txt"
        file <- newFileDefault fakeFile
        ok <- isFileFresh file
        ok `shouldBe` False

    it "isFileFresh returns False for stale file" $
        withSystemTempFile "test.txt" $ \fp h -> do
            hClose h

            file <- newFile fp 0.001
            threadDelay 10000

            ok <- isFileFresh file
            ok `shouldBe` False

    it "getValue returns Nothing if cache is cleared and file is stale" $ do
        withSystemTempFile "test.txt" $ \fp h -> do
            hPutStr h "cached"
            hClose h

            file <- newFile fp 0.001
            _ <- getValue file
            threadDelay 10000

            -- manual cleanup
            writeIORef (cache file) Nothing

            value <- getValue file
            value `shouldBe` Nothing

    it "readContent manually updates the cache" $ do
        withSystemTempFile "test.txt" $ \fp h -> do
            hPutStr h "first"
            hClose h

            file <- newFileDefault fp
            _ <- readContent file
            cached1 <- getCachedContent file
            cached1 `shouldBe` Just "first"

            writeFile fp "second"
            _ <- readContent file
            cached2 <- getCachedContent file
            cached2 `shouldBe` Just "second"

    it "fileAge returns small diff immediately after writing" $
        withSystemTempFile "test.txt" $ \fp h -> do
            hPutStr h "diff"
            hClose h

            file <- newFileDefault fp
            diff <- fileAge file

            diff `shouldSatisfy` (< 1)

    it "getValue returns Just \"\" for empty file" $
        withSystemTempFile "test.txt" $ \fp h -> do
            hClose h
            file <- newFileDefault fp
            value <- getValue file
            value `shouldBe` Just ""
