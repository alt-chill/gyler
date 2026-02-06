{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestUtils.FetchSpec.Template (mkFetchSpecTest) where

import Test.Hspec

import qualified Data.ByteString as BS

import Gyler.FetchSpec

import System.FilePath

import Data.Serialize (decode, encode, Serialize)

import Data.IORef
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)

import Control.Monad (when)

import Type.Reflection (Typeable, typeRep)

mkFetchSpecTest :: forall e. (FetchSpec e, Typeable e) => e -> SpecWith (IORef (Map.Map FilePath String))
mkFetchSpecTest query = describe ("Gyler.FetchSpec." <> typeName) $ do
    describe "parseValue" $ do
        it "matches golden output" $ \_ -> do
            let storeDir = joinPath ["test", "golden", "FetchSpec", typeName]

                inputFile  = storeDir </> "input.txt"
                outputFile = storeDir </> "output.dat"

            input    <- BS.readFile inputFile
            expected <- BS.readFile outputFile

            let Right parsed = parseResult query Nothing input

            case decode expected :: Either String (Result e) of
                Left err        -> expectationFailure $ "decode failed: " <> err
                Right expected' -> parsed `shouldBe` expected'

        it "is Serialize roundtrippable" $ \_ -> do
            let storeDir = joinPath ["test", "golden", "FetchSpec", typeName]
                inputFile  = storeDir </> "input.txt"

            input <- BS.readFile inputFile

            let Right parsed = parseResult query Nothing input
                encoded      = encode parsed
                decoded      = decode encoded

            decoded `shouldBe` Right parsed

    describe "cacheFileName" $ do
        let fname = cacheFileName query

        it "is unique" $ \seenFilesRef -> do
            seen <- liftIO $ readIORef seenFilesRef

            case Map.lookup fname seen of
                Just spec -> expectationFailure $ fname <> " cacheFileName is already used in " <> spec
                Nothing   -> liftIO $ modifyIORef' seenFilesRef (Map.insert fname typeName)

        it "is not empty" $ \_ -> fname `shouldNotBe` ""

        it "contains only filename" $ \_ -> fname `shouldBe` takeFileName fname

    describe "staleAfter" $ do
        let staleTime = staleAfter query

        it "is positive" $ \_ -> staleTime `shouldSatisfy` (>0)

    where
        typeName = show (typeRep @e)
