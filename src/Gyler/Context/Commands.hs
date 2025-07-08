{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the combined configuration for external commands
-- used by gyler: ssh access for gyle/gitery and curl for external calls.

module Gyler.Context.Commands (
    CommandsConfig,
    gyleConfig, giteryConfig,
    curlConfig,
    defCommandsConfig
) where

import Control.Lens (makeLenses)
import Gyler.Context.Ssh
import Gyler.Context.Curl

data CommandsConfig = ExternalCommandsConfig
    { _gyleConfig   :: !SshConfig
    , _giteryConfig :: !SshConfig
    , _curlConfig   :: !CurlConfig
    } deriving (Show, Eq)

makeLenses ''CommandsConfig

defCommandsConfig :: CommandsConfig
defCommandsConfig = ExternalCommandsConfig
    { _gyleConfig   = defSshConfig
    , _giteryConfig = defSshConfig
    , _curlConfig   = defCurlConfig
    }
