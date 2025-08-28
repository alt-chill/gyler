{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Gyler.Domain.Branch (
    Branch,
    BranchesSet(..)
) where

import Gyler.Data.NonEmptyText (NonEmptyText)

import Gyler.Classes.RuntimeValidated.Internal (RuntimeValidated(..))

import Data.Hashable (Hashable)
import Gyler.Data.ValidContainer.HashSet (HashSet)

import GHC.IsList (IsList)
import Data.Serialize (Serialize)

newtype Branch = Branch NonEmptyText
                     deriving Show
                     deriving newtype (Eq, Hashable, Serialize)

instance RuntimeValidated Branch where
    type Raw Branch = NonEmptyText
    mkUnsafe = Branch
    getRaw (Branch x) = x

newtype BranchesSet = BranchesSet (HashSet Branch)
                         deriving Show
                         deriving newtype (Eq, Serialize)
