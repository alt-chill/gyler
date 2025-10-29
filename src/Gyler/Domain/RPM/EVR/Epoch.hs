{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Gyler.Domain.RPM.EVR.Epoch (
    -- Data type.
    Epoch(..),

    -- Parser and smart constructor.
    epochP,
    mkEpoch
) where

-- | Module: Gyler.Domain.RPM.EVR.Epoch
--
-- Description: Optional numerical value which can be used to override normal
-- version-release sorting order.
--
-- Non-existent epoch is exactly equal to zero epoch in all version comparisons.

import Numeric.Natural (Natural)

import Gyler.Parsers (Parser, useParser, nonEmptyText)

import Text.Megaparsec (some, eof)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

import Data.Text (Text)
import qualified Data.Text as T(pack)

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)

import TextShow (showt)

import Gyler.Classes.IsText     (IsText(..))
import Gyler.Classes.Renderable (Renderable(..))

newtype Epoch = Epoch Natural
                    deriving (Eq, Show, Ord, Hashable, Generic)

instance Serialize Epoch

instance Renderable Epoch where
    render (Epoch n) = showt n

epochP :: Parser Epoch
epochP = Epoch <$> (decimal <* char ':')

mkEpoch :: IsText e => e -> Either Text Epoch
mkEpoch txt = useParser (epochP <* eof) (toText txt)
