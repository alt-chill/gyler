module Gyler.Classes.IsText (
    IsText (..)
) where

-- | Module: Gyler.Classes.IsText
--
-- Description: A common abstraction for various Text-based entities.
--
-- In Gyler, many refined newtype wrappers are built around the 'Text' type.
-- The most important one is 'NonEmptyText'.
--
-- Additionally, there are wrappers around 'NonEmptyText' in 'Gyler.Domain'.
--
-- The 'IsText' abstraction is introduced to simplify unwrapping, allowing
-- values to be converted down to the 'Text' level in a single step.
--
-- Module provides IsText instance for Text. (it's just id)

import Data.Text (Text)

class IsText e where
    toText :: e -> Text

instance IsText Text where
    toText = id
