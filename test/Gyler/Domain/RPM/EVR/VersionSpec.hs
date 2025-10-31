{-# LANGUAGE OverloadedStrings #-}

module Gyler.Domain.RPM.EVR.VersionSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T

import Gyler.Domain.RPM.EVR.Version
import Gyler.Classes.IsText (toText)
import Data.Either (isRight, isLeft)

import TestUtils.Serialize.Template (mkSerializeTest)
import Data.Proxy (Proxy(..))

validVersionChar :: Gen Char
validVersionChar = elements $
    ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "._+~"

genValidVersionText :: Gen T.Text
genValidVersionText = T.pack <$> listOf1 validVersionChar

instance Arbitrary Version where
    arbitrary = right . mkVersion <$> genValidVersionText
        where
        right :: Either a b -> b -- partial
        right (Right x) = x

genInvalidVersionText :: Gen T.Text
genInvalidVersionText = T.pack <$> listOf1 (suchThat arbitrary invalidChar)
  where
    invalidChar c = c `notElem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "._+~")

spec :: Spec
spec = parallel $ describe "Version" $ do
    describe "mkVersion" $ do
        it "fails on empty string" $
          mkVersion ("" :: T.Text) `shouldSatisfy` isLeft

        it "Treats '1.2.3' as correct version" $
          mkVersion ("1.2.3" :: T.Text) `shouldSatisfy` isRight

        it "Treats 'v1_0~beta+build' as correct version" $
          mkVersion ("v1_0~beta+build" :: T.Text) `shouldSatisfy` isRight

        it "Fails on '1 2' (spaces are unallowed)" $
          mkVersion ("1 2" :: T.Text) `shouldSatisfy` isLeft

    describe "property-based tests" $ do
        it "returns Right for correct version strings" $
          property $ forAll genValidVersionText $ \txt ->
            isRight $ mkVersion txt

        it "returns Left for strings with invalid chars" $
          property $ forAll genInvalidVersionText $ \txt ->
            isLeft $ mkVersion txt

        it "(mkVersion . toText . fromRight) round-trip" $
          property $ forAll genValidVersionText $ \txt ->
            case mkVersion txt of
              Right v -> mkVersion (toText v) == Right v
              Left  _ -> False


        mkSerializeTest (Proxy :: Proxy Version)
