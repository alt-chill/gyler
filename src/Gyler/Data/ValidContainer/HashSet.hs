module Gyler.Data.ValidContainer.HashSet (
    module Data.HashSet
) where

-- |
-- Module: Gyler.Data.ValidContainer.HashSet
-- Description: ValidContainer and Serialize instance for HashSet from unordered-containers

import Gyler.Data.ValidContainer (ValidContainer(..))

import Data.HashSet
import Data.Serialize (Serialize (..))

import Data.Hashable (Hashable)

instance ValidContainer HashSet where
  vcMember = member
  vcFromList = fromList

instance (Serialize a, Eq a, Hashable a) => Serialize (HashSet a) where
    put = put . toList
    get = fromList <$> get
