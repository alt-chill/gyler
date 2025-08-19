{-# OPTIONS_GHC -Wno-orphans #-}
module Gyler.Data.NonEmptyText.Arbitrary where

import qualified Gyler.Data.NonEmptyText as NEText

import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck (Arbitrary(arbitrary))

instance Arbitrary NEText.NonEmptyText where
      arbitrary = NEText.new <$> arbitrary <*> arbitrary
