module Gyler.Arbitraries.Domain.RPM.EVR.Epoch () where

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Instances.Natural ()

import Gyler.Domain.RPM.EVR.Epoch (Epoch(..))

instance Arbitrary Epoch where
    arbitrary = Epoch <$> arbitrary
