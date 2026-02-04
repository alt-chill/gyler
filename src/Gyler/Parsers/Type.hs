module Gyler.Parsers.Type (Parser) where

-- |
-- Module: Gyler.Parsers
-- Description: Basic Parser type;
--
-- defined in a separate module to avoid cyclic dependencies.

import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec (Parsec)

type Parser = Parsec Void Text
