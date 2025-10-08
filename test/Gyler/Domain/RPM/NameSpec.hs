{-# LANGUAGE OverloadedStrings #-}

module Gyler.Domain.RPM.NameSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T

import Gyler.Domain.RPM.Name
import Gyler.Classes.IsText (toText)
import Data.Maybe (isNothing, isJust)

validChars :: [Char]
validChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-._+"

validNameChar :: Gen Char
validNameChar = elements validChars

genValidNameText :: Gen T.Text
genValidNameText = T.pack <$> listOf1 validNameChar

genInvalidNameText :: Gen T.Text
genInvalidNameText = T.pack <$> listOf1 (suchThat arbitrary invalidChar)
  where
    invalidChar c = c `notElem` validChars

spec :: Spec
spec = parallel $ describe "Name" $ do
    describe "mkName" $ do
        it "fails on empty string" $
          mkName ("" :: T.Text) `shouldBe` Nothing

        it "Threats 'nvidia_glx_470.256.02' as correct package name" $
          mkName ("nvidia_glx_470.256.02" :: T.Text) `shouldSatisfy` (/= Nothing)

        it "Threats 'libgtk+extra2' as correct package name" $
          mkName ("libgtk+extra2" :: T.Text) `shouldSatisfy` (/= Nothing)

        it "Fails on '1 2' (spaces are unallowed)" $
          mkName ("1 2" :: T.Text) `shouldBe` Nothing

    describe "property-based tests" $ do
        it "returns Just for correct verison strings" $
          property $ forAll genValidNameText $ \txt ->
            isJust $ mkName txt

        it "returns Nothing for strings with invalid chars" $
          property $ forAll genInvalidNameText $ \txt ->
            isNothing $ mkName txt

        it "(mkName . toText . fromJust) round-trip" $
          property $ forAll genValidNameText $ \txt ->
            case mkName txt of
              Just v  -> mkName (toText v) == Just v
              Nothing -> False
