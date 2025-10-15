{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Gyler.Domain.RPM.EVR.Version (
    -- Data type. Constructor is not exported.
    Version,

    -- Parser and smart constructor.
    versionP,
    mkVersion
) where

-- | Module: Gyler.Domain.RPM.EVR.Version
--
-- Description: The RPM version string consists of alphanumeric characters, which
-- can optionally be segmented with the separators '.' , '_', '+' and '~'
--
-- Module provides data type with smart constructor.

import Gyler.Data.NonEmptyText (NonEmptyText)

import Gyler.Parsers (Parser, useParser, latinAlphaNum, nonEmptyText)

import Gyler.Domain.RPM.Symbols (versionChars)

import Text.Megaparsec ((<|>), oneOf, some, eof)
import Text.Megaparsec.Char (char)

import Data.Text (Text)
import qualified Data.Text as T(pack)

import GHC.Generics (Generic)

import Data.Hashable (Hashable)
import Data.Serialize (Serialize)

import Gyler.Classes.IsText (IsText(..))

newtype Version = Version NonEmptyText
                     deriving         (Eq, Show, Generic)
                     deriving newtype (Ord, Hashable, Serialize, IsText)

versionSymbols :: Parser Char
versionSymbols = latinAlphaNum <|> oneOf versionChars

versionP :: Parser Version
versionP = Version <$> nonEmptyText (T.pack <$> some versionSymbols)

mkVersion :: IsText e => e -> Maybe Version
mkVersion txt = case useParser (versionP <* eof) (toText txt) of
                Right ver -> Just ver
                Left  _   -> Nothing
