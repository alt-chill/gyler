module Gyler.CachedFileSpec (spec) where

import Gyler.CachedFile.Internal

import Test.Hspec
import System.IO.Temp (withSystemTempFile)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import System.Directory (getModificationTime, removeFile)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, addUTCTime, diffUTCTime)
import System.Process (callProcess)
import System.IO (hClose)
import Data.IORef (writeIORef)
import Data.ByteString (hPut)

spec :: Spec
spec = describe "Gyler.CachedFile" $ do
    it "newFileDefault initializes with empty cache" $ do
        file <- newFileDefault "somefile" ("echo", ["test"])
        cached <- storedData file
        cached `shouldBe` Nothing

    it "updateFile writes and caches the output of command" $
        withSystemTempFile "test.txt" $ \fp _ -> do
            let cmd = ("echo", ["hello"])
            file <- newFile fp cmd 1000
            result <- updateFile file
            cached <- storedData file

            result `shouldBe` BLC.pack "hello\n"
            cached `shouldBe` Just (BLC.pack "hello\n")


    it "getValue calls updateFile if cache is empty and file is outdated" $
        withSystemTempFile "test.txt" $ \fp h -> do
            BL.hPut h (BLC.pack "ignored")
            hClose h

            let cmd = ("echo", ["dynamic"])
            file <- newFile fp cmd 0.1

            threadDelay 100000

            val <- getValue file
            val `shouldBe` BLC.pack "dynamic\n"

    it "getValue reads from cache if it is not outdated" $
        withSystemTempFile "test.txt" $ \fp h -> do
            let content = BLC.pack "cached"
            BL.hPut h content
            hClose h

            file <- newFile fp ("echo", ["not-used"]) 1000

            val <- getValue file
            val `shouldBe` content

            hClose h

    it "isDiffOk returns True for recently modified files" $
        withSystemTempFile "test.txt" $ \fp h -> do
            BL.hPut h (BLC.pack "recent")
            hClose h
            file <- newFile fp ("echo", ["ignore"]) 1000
            ok <- isDiffOk file
            ok `shouldBe` True

    it "isDiffOk returns False for non-existent file" $ do
        let fakeFile = "nonexistent.txt"
        file <- newFile fakeFile ("echo", ["ignore"]) 1000
        ok <- isDiffOk file
        ok `shouldBe` False
