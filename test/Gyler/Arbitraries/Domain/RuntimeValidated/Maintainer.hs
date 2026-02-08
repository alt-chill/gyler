module Gyler.Arbitraries.Domain.RuntimeValidated.Maintainer (

) where

import Gyler.Domain.Maintainer (Maintainer)
import Data.Proxy (Proxy (..))

import Test.QuickCheck (Arbitrary(..))

import TestUtils.RuntimeValidated.Arbitrary (arbitraryRVNonEmptyText)

instance Arbitrary Maintainer where
    arbitrary = arbitraryRVNonEmptyText (Proxy :: Proxy Maintainer)
