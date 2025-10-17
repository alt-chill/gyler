{-# LANGUAGE OverloadedStrings #-}

module Gyler.Domain.RPM.EVRSpec (
    spec
) where

import qualified Gyler.Domain.RPM.EVR.EpochSpec   as Epoch
import qualified Gyler.Domain.RPM.EVR.VersionSpec as Version
import qualified Gyler.Domain.RPM.EVR.ReleaseSpec as Release

import Test.Hspec

import Test.QuickCheck
import Data.Maybe (isJust, isNothing, fromMaybe)

import qualified Data.Text as T

import Gyler.Domain.RPM.EVR
import Gyler.Domain.RPM.EVR.Epoch   (Epoch(..))
import Gyler.Domain.RPM.EVR.Version (mkVersion)
import Gyler.Domain.RPM.EVR.Release (mkRelease)
import Gyler.Classes.IsText (toText)


spec :: Spec
spec = parallel $ describe "EVR" $ do
    Epoch.spec
    Version.spec
    Release.spec

    evrSpec

evrSpec :: Spec
evrSpec = parallel $ do
    describe "mkEvr" $ do
        it "parses full EVR with explicit epoch" $ do
          let evr = mkEvr ("1:1.2.3-4" :: T.Text)
          evr `shouldSatisfy` isJust

        it "parses EVR without epoch (defaults to 0)" $ do
          let evr = mkEvr ("1.2.3-4" :: T.Text)
          evr `shouldBe` Just (EVR (Epoch 0)
                                    (fromMaybe (error "bad version") $ mkVersion ("1.2.3" :: T.Text))
                                    (fromMaybe (error "bad release") $ mkRelease ("4" :: T.Text))
                                    )

        it "fails on invalid EVR (missing release)" $ do
          mkEvr ("1.2.3" :: T.Text) `shouldBe` Nothing

        it "fails on invalid characters" $ do
          mkEvr ("1:1.2.3 4" :: T.Text) `shouldBe` Nothing

        it "fails on empty string" $ do
          mkEvr ("" :: T.Text) `shouldBe` Nothing

    describe "property-based tests" $ do
        it "parses valid EVRs successfully" $
          property $ forAll genValidEvrText $ \txt ->
            isJust (mkEvr txt)

        it "round-trips" $
          property $ forAll genValidEvrText $ \txt ->
            case mkEvr txt of
              Just evr -> mkEvr (showEVR evr) == Just evr
              Nothing  -> False



-- | Generate valid EVR strings such as "1:1.2.3-4", "0:0.1-rc1", "1.0.0-1"
genValidEvrText :: Gen T.Text
genValidEvrText = do
  e <- frequency [(3, pure ""), (1, fmap (\n -> show n <> ":") (choose (0,10000) :: Gen Int))]
  v <- listOf1 $ elements (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "._+~")
  r <- listOf1 $ elements (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "._+~")
  pure . T.pack $ e <> v <> "-" <> r

showEVR :: EVR -> T.Text
showEVR (EVR (Epoch e) v r) = (T.pack . show) e <> ":" <> toText v <> "-" <> toText r
