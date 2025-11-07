module Gyler.Arbitraries.Domain.RPM.EVR.Version () where

import Test.QuickCheck
import qualified Data.Text as T

import Gyler.Domain.RPM.EVR.Version

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

