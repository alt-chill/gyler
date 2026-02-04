module Gyler.Data.ValidContainer (
    ValidContainer(..),
) where

-- |
-- Module: Gyler.Data.ValidContainer
-- Descripton: Abstract interface for different containers
--
-- ValidContainer allows to use different containers with RuntimeValidated
-- typeclass depending on time/space complexity of data structure.

import Data.Hashable (Hashable)

class ValidContainer c where
  vcMember   :: (Eq a, Hashable a) => a -> c a -> Bool
  vcFromList :: (Eq a, Hashable a) => [a] -> c a
  vcToList   :: c a -> [a]
