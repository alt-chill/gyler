{-# LANGUAGE OverloadedStrings #-}

module Gyler.Domain.RPM.EVR.ReleaseSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T

import Gyler.Domain.RPM.EVR.Release
import Gyler.Classes.IsText (toText)
import Data.Either (isRight, isLeft)

import TestUtils.Serialize.Template (mkSerializeTest)
import Data.Proxy (Proxy(..))

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
          mkRelease ("" :: T.Text) `shouldSatisfy` isLeft

        it "Treats '1.2.3' as correct release" $
          mkRelease ("1.2.3" :: T.Text) `shouldSatisfy` isRight

        it "Treats 'v1_0~beta+build' as correct release" $
          mkRelease ("v1_0~beta+build" :: T.Text) `shouldSatisfy` isRight

        it "Fails on '1 2' (spaces are unallowed)" $
          mkRelease ("1 2" :: T.Text) `shouldSatisfy` isLeft

    describe "property-based tests" $ do
        it "returns Right for correct release strings" $
          property $ forAll genValidReleaseText $ \txt ->
            isRight $ mkRelease txt

        it "returns Left for strings with invalid chars" $
          property $ forAll genInvalidReleaseText $ \txt ->
            isLeft $ mkRelease txt

        it "(mkRelease . toText . fromRight) round-trip" $
          property $ forAll genValidReleaseText $ \txt ->
            case mkRelease txt of
              Right v -> mkRelease (toText v) == Right v
              Left  _ -> False

        mkSerializeTest (Proxy :: Proxy Release)
