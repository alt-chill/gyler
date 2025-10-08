{-# OPTIONS_GHC -Wno-orphans #-}

module Gyler.Data.NonEmptyTextSpec (spec) where

import qualified Data.Text as Text
import Data.Maybe (fromMaybe, isNothing, isJust)
import Data.Either (isLeft)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import qualified Gyler.Data.NonEmptyText as NEText

import Gyler.Data.NonEmptyText.Arbitrary ()

import Data.Word (Word8)

import Data.Serialize (Serialize, encode, decode)
import Data.ByteString as BS (pack)

genTextWithTwoSeps :: NEText.NonEmptyText -> Gen Text.Text
genTextWithTwoSeps sep = do
    let s = NEText.toText sep
    size  <- chooseInt (3, 5)
    parts <- vectorOf size arbitrary
    pure $ Text.intercalate s parts

equiv :: Eq a => (NEText.NonEmptyText -> a) -> (Text.Text -> a)
              -> NEText.NonEmptyText -> Bool
equiv fnet ftxt = (==) <$> fnet <*> ftxt . NEText.toText

spec :: Spec
spec = do
    describe "Gyler.Data.NonEmptyText" $ do
        describe "Serialize" $ do
            prop "encode/decode roundtrip" $
                \net -> decode (encode net) == Right (net :: NEText.NonEmptyText)

            it "decode fails on invalid UTF-8 bytes" $ do
                let invalids = [
                       BS.pack [0xC0]
                     , BS.pack [0xC0, 0xAF]
                     , BS.pack [0xFF]
                     ]
                mapM_ (\bs ->
                    (decode bs :: Either String NEText.NonEmptyText)
                      `shouldSatisfy` isLeft) invalids

        describe "breakOnPenultimate" $ do
            prop "If there are fewer than two separators, returns Nothing" $
                \sep t ->
                    let res = NEText.breakOnPenultimate (NEText.singleton sep) t
                        occurrences = Text.count (Text.singleton sep) t
                        length = Text.length t
                    in    (occurrences  < 2 && isNothing res)
                       || (occurrences >= 2)

            prop "Concatenating parts reconstructs original string" $
                \sep -> forAll (genTextWithTwoSeps sep) $ \t ->
                    case NEText.breakOnPenultimate sep t of
                        Just (l, r) ->
                            Text.intercalate (NEText.toText sep)
                                [NEText.toText l, NEText.toText r] == t
                        Nothing -> True

            prop "Right part contains exactly one separator" $
                \sep -> forAll (genTextWithTwoSeps sep) $ \t ->
                    Text.count (NEText.toText sep) t >= 2 ==>
                        case NEText.breakOnPenultimate sep t of
                            Nothing -> True
                            Just (_, r) ->
                                Text.count (NEText.toText sep) (NEText.toText r) == 1
