module Gyler.Arbitraries.Domain.RPM.EVR.Release () where

import Test.QuickCheck
import qualified Data.Text as T

import Gyler.Domain.RPM.EVR.Release

validReleaseChar :: Gen Char
validReleaseChar = elements $
    ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "._+~"

genValidReleaseText :: Gen T.Text
genValidReleaseText = T.pack <$> listOf1 validReleaseChar

instance Arbitrary Release where
    arbitrary = right . mkRelease <$> genValidReleaseText
        where
        right :: Either a b -> b
        right (Right x) = x
