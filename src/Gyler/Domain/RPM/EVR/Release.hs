{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Gyler.Domain.RPM.EVR.Release (
    -- Data type. Constructor is not exported.
    Release,

    -- Parser and smart constructor.
    releaseP,
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

import Gyler.Domain.RPM.Symbols (releaseChars)
import Gyler.Domain.RPM.VerCmp  (rpmvercmp)

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
                     deriving newtype (Hashable, Serialize, IsText)

instance Ord Release where
    compare = rpmvercmp

releaseSymbols :: Parser Char
releaseSymbols = latinAlphaNum <|> oneOf releaseChars

releaseP :: Parser Release
releaseP = Release <$> nonEmptyText (T.pack <$> some releaseSymbols)

mkRelease :: IsText e => e -> Either Text Release
mkRelease txt = useParser (releaseP <* eof) (toText txt)
