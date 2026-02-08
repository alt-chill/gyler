{-# LANGUAGE TypeOperators #-}

module TestUtils.RuntimeValidated.Arbitrary (
    arbitraryRuntimeValidated,
    arbitraryRVNonEmptyText
) where

import Test.QuickCheck (Gen, elements, listOf1)
import Gyler.Classes.RuntimeValidated.Internal (RuntimeValidated(..))

import Gyler.Data.NonEmptyText as NET (NonEmptyText, pack)

import Data.Proxy (Proxy(..))

-- Generator of valid text instances
validChar :: Gen Char
validChar = elements $
    ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

genValidText :: Gen NET.NonEmptyText
genValidText = (\(Just x) -> x)  . NET.pack <$> listOf1 validChar

-------------------------

arbitraryRuntimeValidated :: RuntimeValidated t => Gen (Raw t) -> Gen t
arbitraryRuntimeValidated rawGen = mkUnsafe <$> rawGen

arbitraryRVNonEmptyText :: (RuntimeValidated t, Raw t ~ NonEmptyText) => Proxy r -> Gen t
arbitraryRVNonEmptyText _ = arbitraryRuntimeValidated genValidText
