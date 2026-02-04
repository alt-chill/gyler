module Gyler.Classes.RuntimeValidated.Parser.Raw (
    HasParsableRaw (..)
) where

-- | Module: Gyler.Classes.RuntimeValidated.Parser.Raw
--
-- Description: A typeclass that provides a default parser for the raw value
-- of a RuntimeValidated instance.
--
-- This is mainly needed by `Gyler.Classes.RuntimeValidated.Parser` to
-- construct a universal RuntimeValidated abstract parser.

import Data.Proxy (Proxy)

import Gyler.Classes.RuntimeValidated (RuntimeValidated (Raw))

import Gyler.Parsers.Type (Parser)

-- RV stands for RuntimeValidated
class (RuntimeValidated e) => HasParsableRaw e where
    rawParser :: Proxy e -> Parser (Raw e)
