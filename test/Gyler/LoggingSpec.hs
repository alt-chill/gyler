{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Gyler.LoggingSpec (spec) where

import Test.Hspec
import Gyler.Logging
import Control.Monad
import Control.Monad.Reader
import Control.Exception (bracket)
import qualified Data.Text as T
import System.Directory (removeFile, doesFileExist)
import System.IO (readFile)

import TestUtils (withTempFilePath)

-- Simple environment for HasLogger testing
newtype Env = Env { logger :: LogFunc }

instance HasLogger Env where
    getLogger = logger

withTempLogger :: Severity -> FilePath -> (LogFunc -> IO a) -> IO a
withTempLogger sev fp action =
    bracket (createLogger sev fp) (either (const (return ())) snd) $ \case
            Left err -> error $ "Logger creation failed: " ++ show err
            Right (lf, _) -> action lf

spec :: Spec
spec = describe "Gyler.Logging" $ do
    it "creates a logger and writes an Info message to file" $
        withTempFilePath $ \fp -> do
            exists <- doesFileExist fp
            when exists (removeFile fp)

            withTempLogger Info fp $ \lf -> do
                lf Info "Hello logging"

            content <- readFile fp
            content `shouldContain` "Hello logging"
            content `shouldContain` "[Info]"

    it "does not log messages below minSeverity" $ do
        withTempFilePath $ \fp -> do
            exists <- doesFileExist fp
            when exists (removeFile fp)

            withTempLogger Warning fp $ \lf -> do
                lf Info "This should not appear"
                lf Error "This should appear"

            content <- readFile fp
            content `shouldNotContain` "This should not appear"
            content `shouldContain` "This should appear"

    it "works with logDebug/logInfo/logWarning/logError in Reader env" $ do
        withTempFilePath $ \fp -> do
            exists <- doesFileExist fp
            when exists (removeFile fp)

            withTempLogger Debug fp $ \lf -> do
                let env = Env lf
                runReaderT (do
                    logDebug "debug msg"
                    logInfo "info msg"
                    logWarning "warn msg"
                    logError "error msg") env

            content <- readFile fp
            content `shouldContain` "debug msg"
            content `shouldContain` "info msg"
            content `shouldContain` "warn msg"
            content `shouldContain` "error msg"

    it "createLogger returns Left on invalid file path" $ do
        res <- createLogger Info ""
        case res of
            Left _  -> True `shouldBe` True
            Right _ -> expectationFailure "Expected failure but got Right"
