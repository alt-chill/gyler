{-# LANGUAGE TemplateHaskell #-}

-- | Represents an environment dynamically loaded from a running Gyle/Girar server.
-- Optionally required for the 'parseValue' function of the 'GirarEntity' typeclass.
-- See 'Gyler.Context' and 'Gyler.GirarEntity' for usage details.

module Gyler.GirarEnv (
    GirarEnv,
    -- lenses
    branches, maintainers,
    states
) where

import Gyler.Data.NonEmptyText (NonEmptyText)
import Control.Lens (makeLenses)

-- | All values are retrieved via the 'getData' function from a 'GirarEntity'.
--   Correspondences:
--     - _branches    = getData Gyler.GirarEntity.Branches
--     - _states      = getData Gyler.GirarEntity.States
--     - _maintainers = getData Gyler.GirarEntity.Maintainers
data GirarEnv = GirarEnv
  { _branches    :: ![NonEmptyText]
  , _states      :: ![NonEmptyText]
  , _maintainers :: ![NonEmptyText]
  } deriving (Show, Eq)

makeLenses ''GirarEnv
