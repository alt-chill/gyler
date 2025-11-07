{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Gyler.Arbitraries.Domain.Subtask () where

import Test.QuickCheck

import Gyler.Domain.Subtask
import Gyler.Domain.RPM

-- Arbitrary instances for EVR and Name
import Gyler.Arbitraries.Domain.RPM ()

-- Transitional import
-- Used only to expose the Arbitrary instance for SubtaskType
-- to Gyler.Arbitraries.Domain
import Gyler.Arbitraries.Domain.Subtask.Type ()

-------------------------------------------------------------------------------
-- Arbitrary instances
-------------------------------------------------------------------------------

instance Arbitrary (Subtask 'Gear) where
    arbitrary = GearSubtask <$> (NEVR <$> arbitrary <*> arbitrary)

instance Arbitrary (Subtask 'SRPM) where
    arbitrary = SRPMSubtask <$> (NEVR <$> arbitrary <*> arbitrary)

instance Arbitrary (Subtask 'Del) where
    arbitrary = DelSubtask . N <$> arbitrary

instance Arbitrary SomeSubtask where
    arbitrary = oneof
        [ SomeSubtask <$> (arbitrary :: Gen (Subtask 'Gear))
        , SomeSubtask <$> (arbitrary :: Gen (Subtask 'SRPM))
        , SomeSubtask <$> (arbitrary :: Gen (Subtask 'Del))
        ]
