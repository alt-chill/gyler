{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Configuration for running external SSH commands.
-- Includes executable path, arguments, remote host, user and optionals port
-- and authorization key.

module Gyler.Context.Ssh (
    SshConfig (SshConfig),
    sshExecutable, sshArgs, remoteUser,
    remoteHost, remotePort, authKey,
    defSshConfig
) where

import Control.Lens (makeLenses)
import Gyler.Data.NonEmptyText.Unsafe (NonEmptyText)

data SshConfig = SshConfig
    { _sshExecutable :: !NonEmptyText
    , _sshArgs       :: ![NonEmptyText]
    , _remoteUser    :: !NonEmptyText
    , _remoteHost    :: !NonEmptyText
    , _remotePort    :: !(Maybe NonEmptyText)
    , _authKey       :: !(Maybe NonEmptyText)
    } deriving (Show, Eq)

makeLenses ''SshConfig

defSshConfig :: SshConfig
defSshConfig = SshConfig
    { _sshExecutable = "ssh"
    , _sshArgs       = []
    , _remoteUser    = "user"
    , _remoteHost    = "localhost"
    , _remotePort    = Nothing
    , _authKey       = Nothing
    }
