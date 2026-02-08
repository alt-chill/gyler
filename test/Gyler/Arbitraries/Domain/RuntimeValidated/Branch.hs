module Gyler.Arbitraries.Domain.RuntimeValidated.Branch (

) where

import Gyler.Domain.Branch (Branch)
import Data.Proxy (Proxy (..))

import Test.QuickCheck (Arbitrary(..))

import TestUtils.RuntimeValidated.Arbitrary (arbitraryRVNonEmptyText)

instance Arbitrary Branch where
    arbitrary = arbitraryRVNonEmptyText (Proxy :: Proxy Branch)
