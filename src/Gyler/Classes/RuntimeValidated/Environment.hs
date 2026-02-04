{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gyler.Classes.RuntimeValidated.Environment (
    HasValidSetFor(ValidSet, getValidSet),
    validateRawFromEnv, validateRawFromEnvM
) where

-- | Module: Gyler.Classes.RuntimeValidated.Environment
--
-- Description: A typeclass that provides a way to retrieve a valid set for
-- a `RuntimeValidated` value from a given environment.
--
-- This is mainly needed by `Gyler.Classes.RuntimeValidated.Parser` to
-- construct a universal `RuntimeValidated` abstract parser.
--
-- The container for `ValidSet` should be an instance of the `ValidContainer`
-- typeclass in order to keep the `RuntimeValidated` interface consistent
-- and to enable the use of the `mkValidated` function.
--
-- This invariant is preserved by the default implementation of the
-- `validateRaw'` function, which is not exported from the module and
-- therefore cannot be overridden.
--
-- To define an instance of `HasValidSetFor`, you need to write something
-- like:
--
-- > instance HasValidSetFor SomeRuntimeValidated SomeEnv where
-- >   type ValidSet   = HashSet SomeRuntimeValidated
-- >   getValidSet env = ...

import Data.Proxy (Proxy(..))

import Gyler.Data.ValidContainer (ValidContainer)

import Gyler.Classes.RuntimeValidated.Internal

import Control.Monad.Reader (MonadReader, ask)

-- | Connects a RuntimeValidated type 'rv' with an environment 'e'
class (RuntimeValidated rv) => HasValidSetFor rv e where
    -- injectivity: container type 's' determines element type 'rv',
    -- but allows the same container to be used in multiple environments 'e'.`
    type ValidSet rv e = s | s -> rv
    getValidSet :: e -> ValidSet rv e

    -- Not exported
    validateRaw' :: Proxy e -> ValidSet rv e -> Raw rv -> Maybe rv

    -- Default (and only available) implementation
    default validateRaw'
        :: (ValidSet rv e ~ c rv, ValidContainer c)
        => Proxy e -> ValidSet rv e -> Raw rv -> Maybe rv
    validateRaw' _ = mkValidated

-- Helper that hides the logic for retrieving the valid set from the environment.
validateRawFromEnv :: forall rv e. HasValidSetFor rv e => e -> Raw rv -> Maybe rv
validateRawFromEnv env = validateRaw' (Proxy :: Proxy e) (getValidSet env)

validateRawFromEnvM :: forall rv e m. (HasValidSetFor rv e, MonadReader e m) => Raw rv -> m (Maybe rv)
validateRawFromEnvM raw = do
    env <- ask
    return $ validateRawFromEnv env raw
