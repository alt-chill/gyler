module Gyler.Arbitraries.Domain.RuntimeValidated.State (

) where

import Gyler.Domain.State (State)
import Data.Proxy (Proxy (..))

import Test.QuickCheck (Arbitrary(..))

import TestUtils.RuntimeValidated.Arbitrary (arbitraryRVNonEmptyText)

instance Arbitrary State where
    arbitrary = arbitraryRVNonEmptyText (Proxy :: Proxy State)

