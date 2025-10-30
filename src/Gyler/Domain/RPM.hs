{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Gyler.Domain.RPM (
    EVRFlag(..),
    EVROf(..),
    RPM,
    pattern NEVR,
    pattern N,
) where

-- | Module: Gyler.Domain.RPM
--
-- Description: A simple representation of RPM package metadata,
-- containing only the package name and EVR.

import Gyler.Domain.RPM.EVR  (EVR)
import Gyler.Domain.RPM.Name (Name)

import GHC.Generics (Generic)
import Data.Hashable (Hashable(..))
import Data.Serialize (Serialize(..), Get)

import Gyler.Serialize (deriveIDSerializable)

---------------------------------------------------
-- EVR Presence Type

data EVRFlag = WithEVR | NoEVR deriving (Show, Eq, Generic)

instance Hashable  EVRFlag
instance Serialize EVRFlag

---------------------------------------------------
-- Main Type

-- | RPM is parameterized by the presence of an EVR:
-- + RPM 'WithEVR' stores an 'EVR' value in the 'evr' field
-- + RPM 'NoEVR'   stores    '()'        in the 'evr' field
type family EVROf (f :: EVRFlag) where
    EVROf 'WithEVR = EVR
    EVROf 'NoEVR   = ()

data RPM (h :: EVRFlag) = RPM {
    name :: !Name,
    evr  :: !(EVROf h)
}

---------------------------------------------------
-- Pattern Synonyms

pattern NEVR :: Name -> EVR -> RPM 'WithEVR
pattern NEVR n e = RPM n e

pattern N :: Name -> RPM 'NoEVR
pattern N n = RPM n ()

{-# COMPLETE NEVR, N #-}

---------------------------------------------------
-- Typeclass Instances

deriving instance (Show (EVROf h)) => Show (RPM h)
deriving instance (Eq   (EVROf h)) => Eq   (RPM h)
deriving instance Generic (RPM h)

instance (Hashable  (EVROf h)) => Hashable  (RPM h)

---------------------------------------------------
-- Serialize
--
$(deriveIDSerializable [t| RPM 'WithEVR |])
$(deriveIDSerializable [t| RPM 'NoEVR   |])
