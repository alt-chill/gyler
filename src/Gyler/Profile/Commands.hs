{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the combined configuration for external commands
-- used by gyler: ssh access for gyle/gitery and curl for external calls.

module Gyler.Profile.Commands (
    module Gyler.Profile.Commands.Curl,
    module Gyler.Profile.Commands.Ssh,
    CommandsConfig,
    gyleSsh, giterySsh,
    girarWeb,
    defCommandsConfig
) where

import Control.Lens (makeLenses)
import Gyler.Profile.Commands.Ssh
import Gyler.Profile.Commands.Curl

data CommandsConfig = ExternalCommandsConfig
    { _gyleSsh   :: !(Maybe SshConfig)
    , _giterySsh :: !(Maybe SshConfig)
    , _girarWeb  :: !(Maybe CurlConfig)
    } deriving (Show, Eq)

makeLenses ''CommandsConfig

defCommandsConfig :: CommandsConfig
defCommandsConfig = ExternalCommandsConfig
    { _gyleSsh   = Nothing
    , _giterySsh = Nothing
    , _girarWeb  = Nothing
    }
