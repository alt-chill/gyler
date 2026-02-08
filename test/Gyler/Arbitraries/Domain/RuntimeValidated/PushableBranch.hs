module Gyler.Arbitraries.Domain.RuntimeValidated.PushableBranch (

) where

import Gyler.Domain.PushableBranch (PushableBranch)
import Data.Proxy (Proxy (..))

import Test.QuickCheck (Arbitrary(..))

import TestUtils.RuntimeValidated.Arbitrary (arbitraryRVNonEmptyText)

instance Arbitrary PushableBranch where
    arbitrary = arbitraryRVNonEmptyText (Proxy :: Proxy PushableBranch)

