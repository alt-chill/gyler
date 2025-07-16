{-# LANGUAGE OverloadedStrings #-}

module Gyler.GirarEntity.MaintainersSpec (spec) where

import Test.Hspec

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Gyler.Data.NonEmptyText as NET

import Gyler.GirarEntity
import Gyler.GirarEntity.Maintainers

import Gyler.Context

import Control.Lens

spec :: Spec
spec = describe "Gyler.GirarEntity.Maintainers" $ do
    it "Maintainers parseValue works as expected" $ do
        Just input  <- NET.fromText <$> TIO.readFile "test/golden/GirarEntity/Maintainers/input.txt"
        Just output <- NET.fromText <$> TIO.readFile "test/golden/GirarEntity/Maintainers/output.txt"

        parseValue Maintainers Nothing input `shouldBe` NET.lines output
