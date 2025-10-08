{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Gyler.Domain.RPM.EVR.Release (
    -- Data type. Constructor is not exported.
    Release,

    -- Parser and smart constructor.
    release,
    mkRelease
) where

-- | Module: Gyler.Domain.RPM.EVR.Release
--
-- Description: Package release, used for distinguishing between different
-- builds of the same software version.
--
-- The release string consists of alphanumeric characters can optionally be
-- segmented with the separators '.' , '_', '+' and '~'
--
-- Module provides data type with smart constructor.

import Gyler.Data.NonEmptyText (NonEmptyText)

import Gyler.Parsers (Parser, useParser, latinAlphaNum, nonEmptyText)

import Text.Megaparsec ((<|>), oneOf, some, eof)
import Text.Megaparsec.Char (alphaNumChar, char)

import Data.Text (Text)
import qualified Data.Text as T(pack)

import GHC.Generics (Generic)

import Data.Hashable (Hashable)
import Data.Serialize (Serialize)

import Gyler.Classes.IsText (IsText(..))

newtype Release = Release NonEmptyText
                     deriving         (Eq, Show, Generic)
                     deriving newtype (Ord, Hashable, Serialize, IsText)

releaseSymbols :: Parser Char
releaseSymbols = latinAlphaNum <|> oneOf ("_.+~" :: String)

release :: Parser Release
release = Release <$> nonEmptyText (T.pack <$> some releaseSymbols)

mkRelease :: IsText e => e -> Maybe Release
mkRelease txt = case useParser (release <* eof) (toText txt) of
                Right ver -> Just ver
                Left  _   -> Nothing
