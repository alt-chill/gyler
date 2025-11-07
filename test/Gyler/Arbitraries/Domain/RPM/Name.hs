module Gyler.Arbitraries.Domain.RPM.Name () where

import Test.QuickCheck
import qualified Data.Text as T

import Gyler.Domain.RPM.Name

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

