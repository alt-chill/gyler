{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Gyler.Profile (
    module Gyler.Profile.Commands,
    Profile(Profile),
    commandsConfig, username,
    defProfile
) where

-- Adding a new profile module:
--
-- 1. Create a new module under @Gyler.Profile.<Name>@.
--    The module should define:
--      - A configuration data type (e.g., @FooConfig@),
--      - Default value (e.g., @defFooConfig@),
--      - Lenses via Template Haskell (if needed).
--
-- 2. Update 'GylerProfile' (this file):
--      - Add a new field for the config type,
--      - Extend 'defProfile' accordingly.
--
-- 3. Update 'Gyler.Profile' (this file):
--      - Import the new module,
--      - Re-export it by adding it to the 'module' export list.
--
-- This structure ensures that all context types and defaults are easily
-- accessible throughout the application via a single import:
--
-- > import Gyler.Profile

import Gyler.Profile.Commands

import Gyler.Domain.Maintainer (Maintainer)

import Gyler.Data.NonEmptyText.QQ (net)
import Gyler.Data.NonEmptyText (NonEmptyText)

import Control.Lens (makeLenses)

data Profile = Profile
    { _commandsConfig :: !CommandsConfig

    -- Left NonEmptyText will become Right Maintainer after fetching the valid maintainers set
    , _username        :: !(Either NonEmptyText Maintainer)
    } deriving (Eq, Show)

makeLenses ''Profile

defProfile :: Profile
defProfile = Profile
    { _commandsConfig = defCommandsConfig
    , _username = Left [net|user|]
    }
