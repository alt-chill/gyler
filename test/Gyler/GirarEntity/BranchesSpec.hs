{-# LANGUAGE OverloadedStrings #-}

module Gyler.GirarEntity.BranchesSpec (spec) where

import Test.Hspec

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Gyler.Data.NonEmptyText as NET

import Gyler.GirarEntity
import Gyler.GirarEntity.Branches

import Gyler.Context

import Control.Lens

spec :: Spec
spec = describe "Gyler.GirarEntity.Branches" $ do
    it "Branches parseValue works as expected" $ do
        Just input  <- NET.fromText <$> TIO.readFile "test/golden/GirarEntity/Branches/input.txt"
        Just output <- NET.fromText <$> TIO.readFile "test/golden/GirarEntity/Branches/output.txt"

        parseValue Branches Nothing input `shouldBe` NET.lines output
