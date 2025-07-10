{-# LANGUAGE OverloadedStrings #-}

module Gyler.GirarEntity.MaintainersSpec (spec) where

import Test.Hspec

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Gyler.GirarEntity
import Gyler.GirarEntity.Maintainers

import Gyler.Context

import Control.Lens

spec :: Spec
spec = describe "Gyler.GirarEntity.Maintainers" $ do
    it "Maintainers parseValue works as expected" $ do
        input  <- TIO.readFile "test/golden/GirarEntity/Maintainers/input.txt"
        output <- TIO.readFile "test/golden/GirarEntity/Maintainers/output.txt"

        parseValue Maintainers Nothing input `shouldBe` T.lines output
