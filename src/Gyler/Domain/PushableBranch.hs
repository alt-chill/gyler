{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Gyler.Domain.PushableBranch (
    PushableBranch,
    PushableBranchesSet
) where

import Gyler.Data.NonEmptyText (NonEmptyText)

import Gyler.Classes.RuntimeValidated.Internal (RuntimeValidated(..))

import Data.Hashable (Hashable)
import Gyler.Data.ValidContainer.HashSet (HashSet)

import GHC.IsList (IsList)
import Data.Serialize (Serialize)

newtype PushableBranch = PushableBranch NonEmptyText
                     deriving Show
                     deriving newtype (Eq, Hashable, Serialize)

instance RuntimeValidated PushableBranch where
    type Raw PushableBranch = NonEmptyText
    mkUnsafe = PushableBranch
    getRaw (PushableBranch x) = x

type PushableBranchesSet = HashSet PushableBranch
