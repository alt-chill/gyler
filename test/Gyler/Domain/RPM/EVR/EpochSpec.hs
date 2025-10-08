{-# LANGUAGE OverloadedStrings #-}

module Gyler.Domain.RPM.EVR.EpochSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Natural
import Text.Megaparsec (parse)
import Data.Text (Text, pack)
import Data.Either (isLeft)
import Data.Maybe (isNothing)
import Numeric.Natural (Natural)

import Gyler.Domain.RPM.EVR.Epoch
import Gyler.Parsers (Parser)

-- | Property: mkEpoch (show n <> ":") == Just (Epoch n)
prop_mkEpoch_roundtrip :: Natural -> Bool
prop_mkEpoch_roundtrip n =
  mkEpoch (pack (show n <> ":")) == Just (Epoch n)

-- | Property: mkEpoch should return Nothing for input without ':'
prop_mkEpoch_missing_colon :: Natural -> Bool
prop_mkEpoch_missing_colon n =
  isNothing $ mkEpoch (pack (show n))

spec :: Spec
spec = describe "Epoch" $ do
  describe "mkEpoch" $ do
    it "parses valid input into Just Epoch" $ do
      mkEpoch ("42:" :: Text) `shouldBe` Just (Epoch 42)

    it "parses zero epoch correctly" $ do
      mkEpoch ("0:" :: Text) `shouldBe` Just (Epoch 0)

    it "returns Nothing for missing colon" $ do
      mkEpoch ("123" :: Text) `shouldBe` Nothing

    it "returns Nothing for negative epoch" $ do
      mkEpoch ("-123:" :: Text) `shouldBe` Nothing

    it "returns Nothing for fractional epoch" $ do
      mkEpoch ("3.68:" :: Text) `shouldBe` Nothing

    it "returns Nothing for invalid characters" $ do
      mkEpoch ("abc:" :: Text) `shouldBe` Nothing

    it "returns Nothing for empty string" $ do
      mkEpoch ("" :: Text) `shouldBe` Nothing


  describe "property-based tests (QuickCheck)" $ do
    it "round-trips valid Natural values via mkEpoch" $
      property prop_mkEpoch_roundtrip

    it "returns Nothing when colon is missing" $
      property prop_mkEpoch_missing_colon

