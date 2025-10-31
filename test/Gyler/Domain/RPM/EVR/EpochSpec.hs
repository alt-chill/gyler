{-# LANGUAGE OverloadedStrings #-}

module Gyler.Domain.RPM.EVR.EpochSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Natural
import Data.Text (Text, pack)
import Data.Either (isLeft)

import Numeric.Natural (Natural)

import Gyler.Domain.RPM.EVR.Epoch

import TestUtils.Serialize.Template (mkSerializeTest)
import Data.Proxy (Proxy(..))

-- | Property: mkEpoch (show n <> ":") == Right (Epoch n)
prop_mkEpoch_roundtrip :: Natural -> Bool
prop_mkEpoch_roundtrip n =
  mkEpoch (pack (show n <> ":")) == Right (Epoch n)

-- | Property: mkEpoch should return Left for input without ':'
prop_mkEpoch_missing_colon :: Natural -> Bool
prop_mkEpoch_missing_colon n =
  isLeft $ mkEpoch (pack (show n))

instance Arbitrary Epoch where
    arbitrary = Epoch <$> arbitrary

spec :: Spec
spec = describe "Epoch" $ do
  describe "mkEpoch" $ do
    it "parses valid input into Right Epoch" $ do
      mkEpoch ("42:" :: Text) `shouldBe` Right (Epoch 42)

    it "parses zero epoch correctly" $ do
      mkEpoch ("0:" :: Text) `shouldBe` Right (Epoch 0)

    it "returns Left for missing colon" $ do
      mkEpoch ("123" :: Text) `shouldSatisfy` isLeft

    it "returns Left for negative epoch" $ do
      mkEpoch ("-123:" :: Text) `shouldSatisfy` isLeft

    it "returns Left for fractional epoch" $ do
      mkEpoch ("3.68:" :: Text) `shouldSatisfy` isLeft

    it "returns Left for invalid characters" $ do
      mkEpoch ("abc:" :: Text) `shouldSatisfy` isLeft

    it "returns Left for empty string" $ do
      mkEpoch ("" :: Text) `shouldSatisfy` isLeft


  describe "property-based tests (QuickCheck)" $ do
    it "round-trips valid Natural values via mkEpoch" $
      property prop_mkEpoch_roundtrip

    it "returns Left when colon is missing" $
      property prop_mkEpoch_missing_colon

    mkSerializeTest (Proxy :: Proxy Epoch)
