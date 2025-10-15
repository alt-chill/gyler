{-# LANGUAGE OverloadedStrings #-}

module Gyler.Domain.RPM.VerCmpSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.ByteString (useAsCString)

import System.Process (readProcess)

import Foreign.C.Types
import Foreign.C.String

import Gyler.Domain.RPM.VerCmp (rpmvercmp)

newtype RPMVersion = RPMVersion { unRPM :: T.Text }
  deriving (Show, Eq)

instance Arbitrary RPMVersion where
  arbitrary = RPMVersion <$> genVersionText
  shrink (RPMVersion t) = RPMVersion <$> shrinkText t

genVersionText :: Gen T.Text
genVersionText = do
  parts <- listOf1 genPart  -- at least 1
  pure (T.intercalate "" parts)
  where
    genPart = T.pack <$> listOf1 (elements allowedChars)
    allowedChars = ['0'..'9']
                ++ ['a'..'z']
                ++ ['A'..'Z']
                ++ "_.+~"

shrinkText :: T.Text -> [T.Text]
shrinkText t =
  [ T.take i t | i <- [0 .. T.length t - 1] ]

externalCompare :: T.Text -> T.Text -> IO Ordering
externalCompare a b = do
    out <- readProcess "rpmvercmp" [T.unpack a, T.unpack b] ""

    let ws = words out
    pure $ case () of
      _ | "-1" `elem` ws -> LT
        | "1" `elem` ws -> GT
        | "0" `elem` ws -> EQ
        | otherwise     -> error ("Unexpected output: " ++ out)

spec :: Spec
spec = do
  describe "rpmvercmp" $ do
    it "behaves like system one" $
      property $ \(RPMVersion a) (RPMVersion b) -> ioProperty $ do
        let internal = rpmvercmp a b
        external <- externalCompare a b

        pure $
          counterexample (unlines
            [ "a        = " ++ show a
            , "b        = " ++ show b
            , "internal = " ++ show internal
            , "external = " ++ show external
            ]) $
          internal == external
