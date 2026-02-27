{-# LANGUAGE OverloadedStrings #-}

module Gyler.Profile.Commands.Ssh.Codec (
    sshConfigCodec
) where

import Toml (TomlCodec, (.=))
import qualified Toml (dioptional,arrayOf)

import Gyler.Profile.Commands.Ssh (SshConfig(..), sshExecutable, sshArgs,
                                   remoteUser, remoteHost, remotePort, authKey)

import Gyler.Data.NonEmptyText.Codec (_NonEmptyText, nonEmptyTextCodec)

import Control.Lens (view)

sshConfigCodec :: TomlCodec SshConfig
sshConfigCodec = SshConfig
    <$> nonEmptyTextCodec "executable"             .= view sshExecutable
    <*> Toml.arrayOf _NonEmptyText "args"          .= view sshArgs
    <*> nonEmptyTextCodec "username"               .= view remoteUser
    <*> nonEmptyTextCodec "hostname"               .= view remoteHost
    <*> Toml.dioptional (nonEmptyTextCodec "port") .= view remotePort
    <*> Toml.dioptional (nonEmptyTextCodec "key")  .= view authKey
