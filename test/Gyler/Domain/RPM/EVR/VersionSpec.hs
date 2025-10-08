{-# LANGUAGE OverloadedStrings #-}

module Gyler.Domain.RPM.EVR.VersionSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T

import Gyler.Domain.RPM.EVR.Version
import Gyler.Classes.IsText (toText)
import Data.Maybe (isNothing, isJust)

validVersionChar :: Gen Char
validVersionChar = elements $
    ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "._+~"

genValidVersionText :: Gen T.Text
genValidVersionText = T.pack <$> listOf1 validVersionChar

genInvalidVersionText :: Gen T.Text
genInvalidVersionText = T.pack <$> listOf1 (suchThat arbitrary invalidChar)
  where
    invalidChar c = c `notElem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "._+~")

spec :: Spec
spec = parallel $ describe "Version" $ do
    describe "mkVersion" $ do
        it "fails on empty string" $
          mkVersion ("" :: T.Text) `shouldBe` Nothing

        it "Threats '1.2.3' as correct version" $
          mkVersion ("1.2.3" :: T.Text) `shouldSatisfy` (/= Nothing)

        it "Threats 'v1_0~beta+build' as correct version" $
          mkVersion ("v1_0~beta+build" :: T.Text) `shouldSatisfy` (/= Nothing)

        it "Fails on '1 2' (spaces are unallowed)" $
          mkVersion ("1 2" :: T.Text) `shouldBe` Nothing

    describe "property-based tests" $ do
        it "returns Just for correct verison strings" $
          property $ forAll genValidVersionText $ \txt ->
            isJust $ mkVersion txt

        it "returns Nothing for strings with invalid chars" $
          property $ forAll genInvalidVersionText $ \txt ->
            isNothing $ mkVersion txt

        it "(mkVersion . toText . fromJust) round-trip" $
          property $ forAll genValidVersionText $ \txt ->
            case mkVersion txt of
              Just v  -> mkVersion (toText v) == Just v
              Nothing -> False

