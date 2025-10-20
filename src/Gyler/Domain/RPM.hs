{-# LANGUAGE DeriveGeneric #-}

module Gyler.Domain.RPM (
    RPM(..)
) where

-- | Module: Gyler.Domain.RPM
--
-- Description: A simple representation of RPM package metadata,
-- consisting only of the Name and EVR.

import Gyler.Domain.RPM.EVR  (EVR)
import Gyler.Domain.RPM.Name (Name)

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)

data RPM = RPM {
    _name :: !Name,
    _evr  :: !(Maybe EVR)
} deriving (Show, Eq, Generic)

instance Hashable  RPM
instance Serialize RPM
