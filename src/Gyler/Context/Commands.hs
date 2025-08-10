{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the combined configuration for external commands
-- used by gyler: ssh access for gyle/gitery and curl for external calls.

module Gyler.Context.Commands (
    CommandsConfig,
    gyleSsh, giterySsh,
    girarWeb,
    defCommandsConfig
) where

import Control.Lens (makeLenses)
import Gyler.Context.Ssh
import Gyler.Context.Curl

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
