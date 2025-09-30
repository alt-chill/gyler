{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Gyler.Domain.State (
    State,
    StatesSet
) where

import Gyler.Data.NonEmptyText (NonEmptyText)

import Gyler.Classes.RuntimeValidated.Internal (RuntimeValidated(..))

import Data.Hashable (Hashable)
import Gyler.Data.ValidContainer.HashSet (HashSet)

import Data.Serialize (Serialize)

newtype State = State NonEmptyText
                     deriving Show
                     deriving newtype (Eq, Hashable, Serialize)

instance RuntimeValidated State where
    type Raw State = NonEmptyText
    mkUnsafe = State
    getRaw (State x) = x

type StatesSet = HashSet State
