{-# LANGUAGE TypeFamilies #-}

module Gyler.Classes.RuntimeValidated.Internal (
    RuntimeValidated(..),
    mkValidated, mkValidSet
) where


import Data.Kind (Type)
import Data.Hashable (Hashable)
import Gyler.Data.ValidContainer (ValidContainer, vcMember, vcFromList)

-- Eq and Hashable constraints enable usage with unordered-containers
class (Eq a, Hashable a) => RuntimeValidated a where
  type Raw a :: Type
  mkUnsafe :: Raw a -> a
  getRaw   :: a -> Raw a

-- Attempts to create a validated value from a raw value
-- Checks if the candidate value (built from the raw value) exists in the reference container
-- Returns `Just a` if found, otherwise `Nothing`
mkValidated
  :: (RuntimeValidated a, ValidContainer c, Eq a, Hashable a)
  => c a -> Raw a -> Maybe a
mkValidated container raw =
  let candidate = mkUnsafe raw
  in if vcMember candidate container
       then Just candidate
       else Nothing

-- Creates a container of reference values from a list of raw values
-- Uses the unsafe constructor (mkUnsafe) since raw values are assumed to be valid
mkValidSet
  :: (RuntimeValidated a, ValidContainer c, Eq a, Hashable a)
  => [Raw a]
  -> c a
mkValidSet raws =
  let values = fmap mkUnsafe raws
  in vcFromList values
