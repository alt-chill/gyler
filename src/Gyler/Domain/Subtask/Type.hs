{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Gyler.Domain.Subtask.Type (
    SubtaskType(..),
    HasEVRFor
) where

-- | Module: Gyler.Domain.Subtask.Type
--
-- Description: Types representing variants of Girar subtasks.
--
-- There are only three possible types: Gear, SRPM, and Delete.
-- Any other type (such as copy, rebuild, or kmodules) is a variant
-- of these three with some additional fields.
--
-- Each Gear or SRPM subtask always represents a package with a version.
-- A Delete subtask contains only a package name, without a version.
--
-- This behavior is defined by the type family:
-- HasEVRFor :: SubtaskType -> EVRFlag
--
-- See also: Gyler.Domain.RPM, Gyler.Domain.Subtask

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

import Gyler.Serialize (deriveIDSerializable)

import Gyler.Domain.RPM (EVRFlag(..))

data SubtaskType = Gear | SRPM | Del deriving (Show, Eq, Generic)

instance Hashable  SubtaskType
$(deriveIDSerializable [t| SubtaskType |])

type family HasEVRFor (t :: SubtaskType) :: EVRFlag where
    HasEVRFor 'Gear = 'WithEVR
    HasEVRFor 'SRPM = 'WithEVR
    HasEVRFor 'Del  = 'NoEVR
