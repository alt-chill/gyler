{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -Wno-orphans #-}   -- avoid the warning for the orphan instance

-- | Unsafe helpers for 'NonEmptyText'.
--
-- Importing this module brings an 'IsString' instance into scope, so you can
-- write string literals of type 'NonEmptyText':
--
-- >>> :set -XOverloadedStrings
-- >>> "hello" :: NonEmptyText
-- "hello"
--
-- Beware: the instance throws a runtime error if the literal is empty:
--
-- >>> "" :: NonEmptyText
-- *** Exception: Gyler.Data.NonEmptyText.Unsafe.fromString: empty string literal
--
-- ONLY FOR TESTING PURPOSES
module Gyler.Data.NonEmptyText.Unsafe (
    module Gyler.Data.NonEmptyText   -- re‑export the safe API
  ) where

import Data.String (IsString (..))

import qualified Data.Text as Text (pack)

import Gyler.Data.NonEmptyText

-- | Orphan 'IsString' instance – *unsafe* because it crashes on empty strings.
instance IsString NonEmptyText where
  fromString s =
    case fromText (Text.pack s) of
      Just ne -> ne
      Nothing ->
        error "Gyler.Data.NonEmptyText.Unsafe.fromString: empty string literal"
