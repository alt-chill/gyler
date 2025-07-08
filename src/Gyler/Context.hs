-- | Gyler.Context - central import hub for gyler execution context.
--
-- See Gyler.GylerM.
--
-- Adding a new context module:
--
-- 1. Create a new module under @Gyler.Context.<Name>@.
--    The module should define:
--      - A configuration data type (e.g., @FooConfig@),
--      - Default value (e.g., @defFooConfig@),
--      - Lenses via Template Haskell (if needed).
--
-- 2. Update 'GylerContext' (in @Gyler.Context.Gyler@):
--      - Add a new field for the config type,
--      - Extend 'defContext' accordingly.
--
-- 3. Update 'Gyler.Context' (this file):
--      - Import the new module,
--      - Re-export it by adding it to the 'module' export list.
--
-- This structure ensures that all context types and defaults are easily
-- accessible throughout the application via a single import:
--
-- > import Gyler.Context

module Gyler.Context (
    module Gyler.Context.Ssh,
    module Gyler.Context.Curl,
    module Gyler.Context.Commands,
    module Gyler.Context.Gyler
) where

import Gyler.Context.Ssh
import Gyler.Context.Curl
import Gyler.Context.Commands
import Gyler.Context.Gyler
