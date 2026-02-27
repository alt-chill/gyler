{-# LANGUAGE OverloadedStrings #-}

module Gyler.Profile.Codec (
    profileCodec
) where

-- | Module: Gyler.Profile.Codec
--
-- Description:
--
-- The main module in the hierarchy of configuration TOML codecs.
-- Provides bidirectional transformation between TOML <-> Profile (data type).
--
-- To obtain a TOML representation of the base config, you can use the function:
-- > Toml.encodeToFile profileCodec "path/to/conf.toml" defProfile
--
-- To parse TOML into a Haskell data type, you can use the function:
-- > Toml.decodeFile profileCodec "path/to/conf.toml"
--
-- WARNING!
-- When encoding/decoding the Profile config, validation of the username as
-- the domain data type Maintainer is lost.
--
-- A parsed Profile will always contain Left NonEmptyText in username,
-- even if at the time of encoding it contained Right Maintainer.

import Toml (TomlCodec, (.=), TOML)
import qualified Toml (table, Key, dimap)

import Gyler.Profile (Profile(..), commandsConfig, username)
import Gyler.Profile.Commands.Codec (commandsConfigCodec)

import Gyler.Data.NonEmptyText (NonEmptyText)

import Gyler.Classes.IsNonEmptyText (IsNonEmptyText (..))

import Gyler.Data.NonEmptyText.Codec (nonEmptyTextCodec)

import Control.Lens (view)

profileCodec :: TomlCodec Profile
profileCodec = Profile
    <$> Toml.table commandsConfigCodec "commands"       .= view commandsConfig
    <*> Toml.table (rawAsLeftCodec "name") "maintainer" .= view username
    where
        -- Decode NonEmptyText from TOML and wrap into Left.
        -- Maintainer is a domain value that validated later.
        rawAsLeftCodec :: (IsNonEmptyText a) => Toml.Key -> TomlCodec (Either NonEmptyText a)
        rawAsLeftCodec key = Toml.dimap (either id toNonEmptyText) Left $ nonEmptyTextCodec key
