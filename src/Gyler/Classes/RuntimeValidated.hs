module Gyler.Classes.RuntimeValidated (
    RuntimeValidated(getRaw, Raw),
    mkValidated,
    mkValidSet
) where

-- |
-- Module: Gyler.Classes.RuntimeValidated
-- Description: Abstraction for values that must belong to a reference set
--
-- Useful for datatypes meeting these conditions:
-- 1. The type has a reference set of valid values
-- 2. This set is only available at runtime
-- 3. Instances can only be created if their value exists in the reference set
--
-- All instances of RuntimeValidated should provide an unsafe constructor and
-- a getter for the raw value. For example:
--
-- ```
-- data Maintainer = Maintainer NonEmptyText
--
-- instance RuntimeValidated Maintainer where
--     type Raw Maintainer = NonEmptyText
--     mkUnsafe = Maintainer
--     getRaw (Maintainer x) = x
-- ```
--
-- For instance implementations, the Gyler.Classes.RuntimeValidated.Internal
-- module should be used. The constructor used for mkUnsafe should not be exported.
--
-- When creating a new RuntimeValidated value, the proper approach is to use the
-- mkValidated function. A set of reference values should be created with mkValidSet.
-- Both functions are exposed by the external Gyler.Classes.RuntimeValidated module.
--
-- See: 'Gyler.Domain.Maintainer', 'Gyler.Domain.State', 'Gyler.Domain.Branch'

import Gyler.Classes.RuntimeValidated.Internal
