{-# LANGUAGE OverloadedStrings #-}

module Gyler.GirarEntity.BranchesSpec (spec) where

import Test.Hspec

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Gyler.GirarEntity
import Gyler.GirarEntity.Branches

import Gyler.Context

import Control.Lens

spec :: Spec
spec = describe "Gyler.GirarEntity.Branches" $ do
    it "Branches parseValue works as expected" $ do
        input  <- TIO.readFile "test/golden/GirarEntity/Branches/input.txt"
        output <- TIO.readFile "test/golden/GirarEntity/Branches/output.txt"

        parseValue Branches Nothing input `shouldBe` T.lines output
