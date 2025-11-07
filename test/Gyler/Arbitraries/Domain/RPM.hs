{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Gyler.Arbitraries.Domain.RPM () where

import Gyler.Domain.RPM
import Test.QuickCheck

import Gyler.Arbitraries.Domain.RPM.EVR ()
import Gyler.Arbitraries.Domain.RPM.Name ()

instance Arbitrary (RPM 'WithEVR) where
    arbitrary = NEVR <$> arbitrary <*> arbitrary

instance Arbitrary (RPM 'NoEVR) where
    arbitrary = N <$> arbitrary

