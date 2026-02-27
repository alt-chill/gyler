{-# LANGUAGE OverloadedStrings #-}

module Gyler.Profile.Commands.Curl.Codec (
    curlConfigCodec
) where

import Toml (TomlCodec, (.=))
import qualified Toml (arrayOf)

import Gyler.Profile.Commands.Curl (CurlConfig(..), curlExecutable, fetchAddress)

import Gyler.Data.NonEmptyText.Codec (_NonEmptyText, nonEmptyTextCodec)

import Control.Lens (view)

curlConfigCodec :: TomlCodec CurlConfig
curlConfigCodec = CurlConfig
    <$> nonEmptyTextCodec "executable" .= view curlExecutable
    <*> nonEmptyTextCodec "address"    .= view fetchAddress
