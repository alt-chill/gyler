module Gyler.Classes.IsNonEmptyText (
    IsNonEmptyText (..)
) where

-- | Module: Gyler.Classes.IsNonEmptyText
--
-- Description: A common abstraction for various NonEmptyText-based entities.
--
-- In Gyler, many domain newtype wrappers are built around the 'NonEmptyText' type.
--
-- The 'IsNonEmptyText' abstraction is introduced to simplify unwrapping, allowing
-- values to be converted down to the 'NonEmptyText' level in a single step.
--
-- Module provides IsNonEmptyText instance for NonEmptyText. (it's just id)

import Gyler.Data.NonEmptyText (NonEmptyText)

class IsNonEmptyText e where
    toNonEmptyText :: e -> NonEmptyText

instance IsNonEmptyText NonEmptyText where
    toNonEmptyText = id
