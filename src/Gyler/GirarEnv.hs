{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

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

import Gyler.Domain.Branch (BranchesSet, Branch)
import Gyler.Domain.State (StatesSet, State)
import Gyler.Domain.Maintainer (MaintainersSet, Maintainer)

import Gyler.Classes.RuntimeValidated.Environment (HasValidSetFor(..))

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

instance HasValidSetFor Branch GirarEnv where
    type ValidSet Branch GirarEnv = BranchesSet
    getValidSet = _branches

instance HasValidSetFor State GirarEnv where
    type ValidSet State GirarEnv = StatesSet
    getValidSet = _states

instance HasValidSetFor Maintainer GirarEnv where
    type ValidSet Maintainer GirarEnv = MaintainersSet
    getValidSet = _maintainers
