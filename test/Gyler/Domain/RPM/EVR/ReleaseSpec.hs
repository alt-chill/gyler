{-# LANGUAGE OverloadedStrings #-}

module Gyler.Domain.RPM.EVR.ReleaseSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T

import Gyler.Domain.RPM.EVR.Release
import Gyler.Classes.IsText (toText)
import Data.Maybe (isNothing, isJust)

validReleaseChar :: Gen Char
validReleaseChar = elements $
    ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "._+~"

genValidReleaseText :: Gen T.Text
genValidReleaseText = T.pack <$> listOf1 validReleaseChar

genInvalidReleaseText :: Gen T.Text
genInvalidReleaseText = T.pack <$> listOf1 (suchThat arbitrary invalidChar)
  where
    invalidChar c = c `notElem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "._+~")

spec :: Spec
spec = parallel $ describe "Release" $ do
    describe "mkRelease" $ do
        it "fails on empty string" $
          mkRelease ("" :: T.Text) `shouldBe` Nothing

        it "Threats '1.2.3' as correct release" $
          mkRelease ("1.2.3" :: T.Text) `shouldSatisfy` (/= Nothing)

        it "Threats 'v1_0~beta+build' as correct release" $
          mkRelease ("v1_0~beta+build" :: T.Text) `shouldSatisfy` (/= Nothing)

        it "Fails on '1 2' (spaces are unallowed)" $
          mkRelease ("1 2" :: T.Text) `shouldBe` Nothing

    describe "property-based tests" $ do
        it "returns Just for correct verison strings" $
          property $ forAll genValidReleaseText $ \txt ->
            isJust $ mkRelease txt

        it "returns Nothing for strings with invalid chars" $
          property $ forAll genInvalidReleaseText $ \txt ->
            isNothing $ mkRelease txt

        it "(mkRelease . toText . fromJust) round-trip" $
          property $ forAll genValidReleaseText $ \txt ->
            case mkRelease txt of
              Just v  -> mkRelease (toText v) == Just v
              Nothing -> False

