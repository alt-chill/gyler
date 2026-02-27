{-# LANGUAGE OverloadedStrings #-}

module Gyler.Data.NonEmptyText.Codec (
    _NonEmptyText,
    nonEmptyTextCodec
) where

import Toml (TomlCodec)
import qualified Toml

import Gyler.Data.NonEmptyText (NonEmptyText, toText, fromText)

_NonEmptyText :: Toml.TomlBiMap NonEmptyText Toml.AnyValue
_NonEmptyText = Toml._TextBy
    toText
    (maybe (Left "Text cannot be empty") Right . fromText)


nonEmptyTextCodec :: Toml.Key -> TomlCodec NonEmptyText
nonEmptyTextCodec = Toml.match _NonEmptyText
