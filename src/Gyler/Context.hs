{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

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
-- 2. Update 'GylerContext' (this file):
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
    GylerContext(..),
    defContext,
    commandsConfig, girarEnv, altUser,
    cacheDir
) where

import Gyler.Context.Ssh
import Gyler.Context.Curl
import Gyler.Context.Commands

import Control.Lens (makeLenses)

import Gyler.GirarEnv (GirarEnv)

import Gyler.Data.NonEmptyText (NonEmptyText)
import Gyler.Data.NonEmptyText.QQ (net)

data GylerContext = GylerContext
    { _commandsConfig :: !CommandsConfig
    , _girarEnv       :: !(Maybe GirarEnv)
    , _cacheDir       :: !FilePath
    , _altUser        :: !NonEmptyText
    } deriving (Show, Eq)

makeLenses ''GylerContext

defContext :: GylerContext
defContext = GylerContext
    { _commandsConfig = defCommandsConfig
    , _girarEnv = Nothing
    , _cacheDir = "/tmp"
    , _altUser = [net|user|]
    }
