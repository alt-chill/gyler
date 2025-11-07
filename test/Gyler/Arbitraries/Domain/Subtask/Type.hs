module Gyler.Arbitraries.Domain.Subtask.Type () where

import Test.QuickCheck

import Gyler.Domain.Subtask.Type

instance Arbitrary SubtaskType where
    arbitrary = elements [Gear, SRPM, Del]
