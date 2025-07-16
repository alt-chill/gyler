{-# LANGUAGE OverloadedStrings #-}

module Gyler.CachedFileSpec (spec) where

import Gyler.CachedFile.Internal

import Test.Hspec

import qualified Data.Text    as T (Text)

import System.Directory (getModificationTime, removeFile)
import System.Process (callProcess)
import System.IO (hClose, hPutStr, hPutStrLn)
import System.IO.Temp (withSystemTempFile)

import Data.IORef (writeIORef)
import Data.Either (isLeft)
import Data.Time.Clock (getCurrentTime, addUTCTime, diffUTCTime, NominalDiffTime (..))

import Control.Exception (try)
import Control.Concurrent (threadDelay)

import TestUtils (withTempFilePath)

import Gyler.Data.NonEmptyText.Unsafe (NonEmptyText)

spec :: Spec
spec = describe "Gyler.CachedFile" $ do
    describe "fileAge" $ do
        it "returns small diff immediately after writing" $
            withSystemTempFile "test.txt" $ \fp h -> do
                hPutStr h "diff"
                hClose h

                file <- newFileDefault fp
                diff <- fileAge file

                diff `shouldSatisfy` (< 1)

        it "throws exception for nonexistent file" $ do
            file <- newFileDefault "nonexistent.txt"
            result <- try (fileAge file) :: IO (Either IOError NominalDiffTime)
            result `shouldSatisfy` isLeft

    describe "fetchOrRun" $ do
        it "caches empty output properly" $
            withTempFilePath $ \fp -> do
                file <- newFileDefault fp
                out <- fetchOrRun file ("true", [])
                out `shouldBe` (Executable, "")
                cached <- getCachedContent file
                cached `shouldBe` Just ""

        it "returns \"\" if output is not valid UTF-8" $
            withTempFilePath $ \fp -> do
                file <- newFileDefault fp
                out <- fetchOrRun file ("head", ["-c", "1000", "/dev/urandom"])
                out `shouldBe` (Error, "")

        it "returns cached value if present" $
            withTempFilePath $ \fp -> do
                file <- newFileDefault fp
                writeValue file "cached!"
                out <- fetchOrRun file ("echo", ["ignored"])
                out `shouldBe` (Cache, "cached!")

        it "returns empty string if command fails" $
            withTempFilePath $ \fp -> do
                file <- newFileDefault fp
                out <- fetchOrRun file ("false", [])
                out `shouldBe` (Error, "")

        it "returns empty string if executable does not exist" $
            withTempFilePath $ \fp -> do
                file <- newFileDefault fp
                out <- fetchOrRun file ("unknown_command_", [])
                out `shouldBe` (Error, "")

        it "runs command and caches output if no cache is present" $
            withTempFilePath $ \fp -> do
                file <- newFileDefault fp
                out1 <- fetchOrRun file ("echo", ["hello"])
                out1 `shouldBe` (Executable, "hello\n")

                content <- readFile fp
                content `shouldBe` "hello\n"

                out2 <- fetchOrRun file ("echo", ["ignored"])
                out2 `shouldBe` (Cache, "hello\n")

        it "runs command and return output if file is not available" $ do
            file <- newFileDefault "/non/existing/dir/file.txt"
            out1 <- fetchOrRun file ("echo", ["hello"])
            out1 `shouldBe` (Executable, "hello\n")

        it "runs command if file is stale" $
            withSystemTempFile "test.txt" $ \fp h -> do
                hPutStr h "stale"
                hClose h

                file <- newFile fp 0.001
                threadDelay 10000

                out <- fetchOrRun file ("echo", ["fresh"])
                out `shouldBe` (Executable, "fresh\n")

    describe "isFileFresh" $ do
        it "returns False for non-existent file" $ do
            let fakeFile = "nonexistent.txt"
            file <- newFileDefault fakeFile
            ok <- isFileFresh file
            ok `shouldBe` False

        it "returns False for stale file" $
            withSystemTempFile "test.txt" $ \fp h -> do
                hClose h

                file <- newFile fp 0.001
                threadDelay 10000

                ok <- isFileFresh file
                ok `shouldBe` False

        it "returns True for recently modified files" $ do
            withSystemTempFile "test.txt" $ \fp h -> do
                hPutStr h "recent"
                hClose h

                file <- newFile fp 1000
                ok <- isFileFresh file

                ok `shouldBe` True

    describe "newFileDefault" $ do
        it "initializes with empty cache" $ do
            file <- newFileDefault "somefile"
            cached <- getCachedContent file
            cached `shouldBe` Nothing

    describe "readCached" $ do
        it "always return cached value after reading" $
            withSystemTempFile "test.txt" $ \fp h -> do
                hPutStr h "hello\n"
                hClose h

                file <- newFileDefault fp

                value  <- readCached file
                cached <- getCachedContent file

                value `shouldBe` Just "hello\n"
                cached `shouldBe` Just "hello\n"

                writeFile fp "ignored"

                value2  <- readCached file
                value2 `shouldBe` value

        it "reads and caches content of file" $
            withSystemTempFile "test.txt" $ \fp h -> do
                hPutStr h "hello\n"
                hClose h

                file <- newFile fp 1000

                value  <- readCached file
                cached <- getCachedContent file

                value `shouldBe` Just "hello\n"
                cached `shouldBe` Just "hello\n"

        it "returns Just \"\" for empty file" $
            withSystemTempFile "test.txt" $ \fp h -> do
                hClose h

                file <- newFileDefault fp
                value <- readCached file
                value `shouldBe` Just ""

        it "returns Nothing for non-existing file" $ do
            file <- newFileDefault "somefile"
            value <- readCached file
            value `shouldBe` Nothing

        it "returns Nothing for stale file" $ do
            withSystemTempFile "test.txt" $ \fp h -> do
                hPutStrLn h "Ignored"
                hClose h

                file <- newFile fp 0.001
                threadDelay 10000

                value <- readCached file
                value `shouldBe` Nothing

        it "returns Nothing if cache is cleared and file is stale" $ do
            withSystemTempFile "test.txt" $ \fp h -> do
                hPutStr h "cached"
                hClose h

                file <- newFile fp 0.001
                _ <- readCached file
                threadDelay 10000

                writeIORef (cache file) Nothing

                value <- readCached file
                value `shouldBe` Nothing

    describe "readContent" $ do
        it "does not clear cache if reading fails" $
            withSystemTempFile "test.txt" $ \fp h -> do
                hPutStr h "good"
                hClose h

                file <- newFileDefault fp
                _ <- readContent file

                removeFile fp
                failed <- readContent file
                cached <- getCachedContent file

                cached `shouldBe` Just "good"
                failed `shouldBe` Nothing

        it "manually updates the cache" $ do
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

        it "returns Nothing for missing file" $ do
            file <- newFileDefault "nonexistent.txt"
            value <- readContent file
            value `shouldBe` Nothing

    describe "writeValue" $ do
        it "catches exception and still updates cache" $ do
            let path = "/forbidden"
            file <- newFileDefault path
            writeValue file "shouldNotFail"
            cached <- getCachedContent file
            cached `shouldBe` Just "shouldNotFail"

        it "updates both cache and file content" $
            withTempFilePath $ \fp -> do
                file <- newFileDefault fp
                writeValue file "written"
                content <- readFile fp
                cached <- getCachedContent file
                content `shouldBe` "written"
                cached `shouldBe` Just "written"
