{-# LANGUAGE TemplateHaskell #-}

-- | Represents an environment dynamically loaded from a running Gyle/Girar server.
-- Optionally required for the 'parseResult' function of the 'FetchSpec' typeclass.
-- See 'Gyler.Context' and 'Gyler.FetchSpec' for usage details.

module Gyler.GirarEnv (
    GirarEnv,
    -- lenses
    branches, maintainers,
    states
) where

import Control.Lens (makeLenses)

import Gyler.Domain.Branch (BranchesSet)
import Gyler.Domain.State (StatesSet)
import Gyler.Domain.Maintainer (MaintainersSet)

-- | All values are retrieved via the fetch function of a 'FetchSpec'.
--   Correspondences:
--     - _branches    = fetch Gyler.FetchSpec.BranchesQuery
--     - _states      = fetch Gyler.FetchSpec.StatesQuery
--     - _maintainers = fetch Gyler.FetchSpec.MaintainersQuery
data GirarEnv = GirarEnv
  { _branches    :: !BranchesSet
  , _states      :: !StatesSet
  , _maintainers :: !MaintainersSet
  } deriving (Show, Eq)

makeLenses ''GirarEnv
