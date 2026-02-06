{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Gyler.Arbitraries.Domain.Task (

) where

import Gyler.Domain.Task (Task(..), TaskView(..), pattern IDOnlyTask)

import Test.QuickCheck

instance Arbitrary TaskView where
    arbitrary = elements [minBound .. maxBound]

instance Arbitrary (Task 'IDOnly) where
    arbitrary = IDOnlyTask <$> arbitrary
