{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Gyler.Domain.RPM.Name (
    -- Data type. Constructor is not exported.
    Name,

    -- Parser and smart constructor.
    nameP,
    mkName
) where

-- | Module: Gyler.Domain.RPM.Name
--
-- Description: The RPM package name string consists of alphanumeric characters, which
-- can optionally be segmented with the separators '-', '.', '_' and '+'
--
-- Module provides data type with smart constructor.

import Gyler.Data.NonEmptyText (NonEmptyText)

import Gyler.Parsers (Parser, useParser, latinAlphaNum, nonEmptyText)

import Text.Megaparsec ((<|>), oneOf, some, eof)
import Text.Megaparsec.Char (char)

import Data.Text (Text)
import qualified Data.Text as T(pack)

import GHC.Generics (Generic)

import Data.Hashable (Hashable)
import Data.Serialize (Serialize)

import Gyler.Classes.IsText (IsText(..))

newtype Name = Name NonEmptyText
                     deriving         (Eq, Show, Generic)
                     deriving newtype (Ord, Hashable, Serialize, IsText)

nameSymbols :: Parser Char
nameSymbols = latinAlphaNum <|> oneOf ("-._+" :: String)

nameP :: Parser Name
nameP = Name <$> nonEmptyText (T.pack <$> some nameSymbols)

mkName :: IsText e => e -> Maybe Name
mkName txt = case useParser (nameP <* eof) (toText txt) of
                Right ver -> Just ver
                Left  _   -> Nothing
