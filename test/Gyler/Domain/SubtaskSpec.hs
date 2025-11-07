{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Gyler.Domain.SubtaskSpec (
    spec
) where

import Test.Hspec
import Test.QuickCheck
import Data.Proxy (Proxy(..))
import qualified Data.Text as T

import Gyler.Domain.Subtask
import Gyler.Domain.RPM (RPM(..), pattern N, pattern NEVR)
import Gyler.Domain.RPM.Name (mkName)
import Gyler.Domain.RPM.EVR (mkEvr)
import Gyler.Classes.Renderable (Renderable(..))

import Text.Megaparsec (parse)
import Gyler.Parsers (useParser)

import Gyler.Arbitraries ()

-------------------------------------------------------------------------------
-- Unified parser test
-------------------------------------------------------------------------------

mkParseExample
    :: SubtaskType
    -> T.Text
    -> Expectation
mkParseExample expectedType input =
    case useParser subtaskParser input of
      Right (SomeSubtask subtask) -> do
          subtaskType subtask `shouldBe` expectedType
          render subtask `shouldBe` input
      Left err ->
          expectationFailure $ "Unexpected parse result\n" <> T.unpack err

-------------------------------------------------------------------------------
-- Typeclass for generalized round-trip test
-------------------------------------------------------------------------------

class (Eq a, Show a, Renderable a) => HasParser a where
    parserFor :: T.Text -> Either T.Text a

instance HasParser (Subtask 'Gear) where
    parserFor = useParser gearParser

instance HasParser (Subtask 'SRPM) where
    parserFor = useParser srpmParser

instance HasParser (Subtask 'Del) where
    parserFor = useParser delParser

instance HasParser SomeSubtask where
    parserFor = useParser subtaskParser

-------------------------------------------------------------------------------
-- Spec
-------------------------------------------------------------------------------

spec :: Spec
spec = parallel $ describe "Subtask" $ do

    ----------------------------------------------------------------------------
    describe "Parsing base examples" $ do
        it "Gear" $
            mkParseExample Gear "tlpui.git=1.8.0-alt1"

        it "SRPM" $
            mkParseExample SRPM "srpm=libgtksourceview3-3.24.11-alt2.src.rpm"

        it "Del" $
            mkParseExample Del "del=pandoc"

    ----------------------------------------------------------------------------
    describe "render . parse == id" $ do
        let roundTripTest :: forall a. (Arbitrary a, HasParser a) => Proxy a -> Property
            roundTripTest _ = property $ \(x :: a) ->
                parserFor (render x) == Right x

        it "Gear" $
            roundTripTest (Proxy @(Subtask 'Gear))

        it "SRPM" $
            roundTripTest (Proxy @(Subtask 'SRPM))

        it "Del" $
            roundTripTest (Proxy @(Subtask 'Del))

        it "SomeSubtask" $
            roundTripTest (Proxy @SomeSubtask)
