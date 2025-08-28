{-# LANGUAGE OverloadedStrings #-}

module Gyler.FetchSpec.BranchesQuerySpec (spec) where

import Test.Hspec

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Gyler.Data.NonEmptyText as NET

import Gyler.FetchSpec
import Gyler.FetchSpec.BranchesQuery

import Gyler.Context

import Gyler.Domain.Branch

import Control.Lens

import qualified Data.ByteString as BS

import Gyler.Classes.RuntimeValidated

import Gyler.Data.ValidContainer.HashSet (toList)

spec :: Spec
spec = describe "Gyler.FetchSpec.BranchesQuery" $ do
    it "Branches parseValue works as expected" $ do
        input  <- BS.readFile "test/golden/FetchSpec/Branches/input.txt"

        Just expected <- fmap NET.lines . NET.fromText <$> TIO.readFile "test/golden/FetchSpec/Branches/output.txt"

        let Just parsed = parseResult BranchesQuery Nothing input

            netList = getRaw <$> toList parsed

        netList `shouldMatchList` expected
