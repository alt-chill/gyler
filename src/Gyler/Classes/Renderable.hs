{-# LANGUAGE DefaultSignatures #-}

module Gyler.Classes.Renderable (
    Renderable(..)
) where

import Data.Text (Text)

import Gyler.Classes.IsText (IsText(..))

-- | Module: Gyler.Classes.Renderable
--
-- Description: A common abstraction for pretty-printing values.
--
-- The 'Show' typeclass is intended primarily for internal or debugging use,
-- producing valid Haskell syntax representations of values.
-- In contrast, 'Renderable' should be used to generate human-readable,
-- nicely formatted output suitable for display to end users.

class Renderable e where
    render :: e -> Text

    default render :: IsText e => e -> Text
    render = toText
