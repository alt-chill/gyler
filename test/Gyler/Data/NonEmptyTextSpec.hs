{-# OPTIONS_GHC -Wno-orphans #-}

module Gyler.Data.NonEmptyTextSpec (spec) where

import qualified Data.Text as Text
import Data.Maybe (fromMaybe)
import Data.Either (isLeft)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import qualified Gyler.Data.NonEmptyText as NEText

import Gyler.Data.NonEmptyText.Arbitrary ()

import Data.Word (Word8)

import Data.Serialize (Serialize, encode, decode)
import Data.ByteString as BS (pack)

equiv :: Eq a => (NEText.NonEmptyText -> a) -> (Text.Text -> a)
              -> NEText.NonEmptyText -> Bool
equiv fnet ftxt = (==) <$> fnet <*> ftxt . NEText.toText

spec :: Spec
spec = do
    describe "Gyler.Data.NonEmptyText" $ do
        prop "toText . NEText.singleton == Text.singleton" $
            (==) <$> NEText.toText . NEText.singleton <*> Text.singleton

        prop "isSingleton . singleton" $
            NEText.isSingleton . NEText.singleton

        prop "toText is never empty" $
            \t -> Text.length (NEText.toText t) > 0

        prop "fromMaybe \"\" (toText <$> fromText Text) == Text" $
            \t ->
                let net :: Maybe Text.Text
                    net = (NEText.toText <$> NEText.fromText t)
                in fromMaybe Text.empty net == t

        prop "length = length . toText" $
            equiv NEText.length Text.length

        prop "(NEText + NEText) = (Text + Text)" $
            \t1 t2 ->
                let net = NEText.append t1 t2
                    txt = Text.append (NEText.toText t1) (NEText.toText t2)
                in NEText.toText net == txt

        prop "head = head . toText" $
            equiv NEText.head Text.head

        prop "last = last . toText" $
            equiv NEText.last Text.last

        prop "tail = tail . toText" $
            equiv NEText.tail Text.tail

        prop "init = init . toText" $
            equiv NEText.init Text.init

    describe "Serialize NonEmptyText" $ do
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
