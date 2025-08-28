{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Gyler.Domain.Maintainer (
    Maintainer,
    MaintainersSet
) where

-- |
-- Module: Gyler.Domain.Maintainer
-- Description: A RuntimeValidated datatype containing maintainers recognized by girar.
--
-- Use RuntimeValidated `mkValidate` to create a new value.
-- Use `fetch` with `MaintainersQuery` to obtain a `MaintainersSet`.
--
-- See: `Gyler.FetchSpec.MaintainersQuery`

import Gyler.Data.NonEmptyText (NonEmptyText)

import Gyler.Classes.RuntimeValidated.Internal (RuntimeValidated(..))

import Data.Hashable (Hashable)
import Gyler.Data.ValidContainer.HashSet (HashSet)

import GHC.IsList (IsList)
import Data.Serialize (Serialize)

newtype Maintainer = Maintainer NonEmptyText
                     deriving Show
                     deriving newtype (Eq, Hashable, Serialize)

instance RuntimeValidated Maintainer where
    type Raw Maintainer = NonEmptyText
    mkUnsafe = Maintainer
    getRaw (Maintainer x) = x

type MaintainersSet = HashSet Maintainer
