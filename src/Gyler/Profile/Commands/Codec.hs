{-# LANGUAGE OverloadedStrings #-}

module Gyler.Profile.Commands.Codec (
    commandsConfigCodec
) where

import Toml (TomlCodec, (.=))
import qualified Toml (table, dioptional)

import Gyler.Profile.Commands.Curl.Codec (curlConfigCodec)
import Gyler.Profile.Commands.Ssh.Codec  (sshConfigCodec)

import Gyler.Profile.Commands (CommandsConfig(..), gyleSsh, giterySsh, girarWeb)

import Control.Lens (view)

commandsConfigCodec :: TomlCodec CommandsConfig
commandsConfigCodec = CommandsConfig
    <$> Toml.table (Toml.dioptional sshConfigCodec)  "gyle"      .= view gyleSsh
    <*> Toml.table (Toml.dioptional sshConfigCodec)  "gitery"    .= view giterySsh
    <*> Toml.table (Toml.dioptional curlConfigCodec) "girar_web" .= view girarWeb
