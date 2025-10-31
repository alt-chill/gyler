{-# LANGUAGE OverloadedStrings #-}

module Gyler.Domain.RPM.NameSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T

import Gyler.Domain.RPM.Name
import Gyler.Classes.IsText (toText)
import Data.Either (isRight, isLeft)

import TestUtils.Serialize.Template (mkSerializeTest)
import Data.Proxy (Proxy(..))

validChars :: [Char]
validChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-._+"

validNameChar :: Gen Char
validNameChar = elements validChars

genValidNameText :: Gen T.Text
genValidNameText = T.pack <$> listOf1 validNameChar

instance Arbitrary Name where
    arbitrary = right . mkName <$> genValidNameText
        where
        right :: Either a b -> b -- partial
        right (Right x) = x

genInvalidNameText :: Gen T.Text
genInvalidNameText = T.pack <$> listOf1 (suchThat arbitrary invalidChar)
  where
    invalidChar c = c `notElem` validChars

spec :: Spec
spec = parallel $ describe "Name" $ do
    describe "mkName" $ do
        it "fails on empty string" $
          mkName ("" :: T.Text) `shouldSatisfy` isLeft

        it "Treats 'nvidia_glx_470.256.02' as correct package name" $
          mkName ("nvidia_glx_470.256.02" :: T.Text) `shouldSatisfy` isRight

        it "Treats 'libgtk+extra2' as correct package name" $
          mkName ("libgtk+extra2" :: T.Text) `shouldSatisfy` isRight

        it "Fails on '1 2' (spaces are unallowed)" $
          mkName ("1 2" :: T.Text) `shouldSatisfy` isLeft

    describe "property-based tests" $ do
        it "returns Right for correct name strings" $
          property $ forAll genValidNameText $ \txt ->
            isRight $ mkName txt

        it "returns Left for strings with invalid chars" $
          property $ forAll genInvalidNameText $ \txt ->
            isLeft $ mkName txt

        it "(mkName . toText . fromRight) round-trip" $
          property $ forAll genValidNameText $ \txt ->
            case mkName txt of
              Right v -> mkName (toText v) == Right v
              Left  _ -> False

        mkSerializeTest (Proxy :: Proxy Name)
