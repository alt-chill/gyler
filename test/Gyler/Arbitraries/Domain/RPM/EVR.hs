module Gyler.Arbitraries.Domain.RPM.EVR () where

import Gyler.Arbitraries.Domain.RPM.EVR.Epoch ()
import Gyler.Arbitraries.Domain.RPM.EVR.Version ()
import Gyler.Arbitraries.Domain.RPM.EVR.Release ()

import Test.QuickCheck

import qualified Data.Text as T

import Gyler.Domain.RPM.EVR

-- | Generate valid EVR strings such as "1:1.2.3-4", "0.1-rc1", "1.0.0-1"
genValidEvrText :: Gen T.Text
genValidEvrText = do
  e <- frequency [(3, pure ""), (1, fmap (\n -> show n <> ":") (choose (0,10000) :: Gen Int))]
  v <- listOf1 $ elements (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "._+~")
  r <- listOf1 $ elements (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "._+~")
  pure . T.pack $ e <> v <> "-" <> r

instance Arbitrary EVR where
    arbitrary = right . mkEvr <$> genValidEvrText
        where
        right :: Either a b -> b -- partial
        right (Right x) = x

