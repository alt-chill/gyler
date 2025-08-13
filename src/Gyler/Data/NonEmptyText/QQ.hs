{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compile-time builders for 'NonEmptyText'.
--
-- Importing this module enables the use of [net|...|] syntax to create
-- 'NonEmptyText' values from string literals.
--
-- Non-emptiness is guaranteed at compile time.
--
-- Requires the QuasiQuotes language extension.
--
-- {-# LANGUAGE QuasiQuotes #-}
--
-- ```
-- ghci> [net|Hello|]
-- "Hello"
-- ghci> :t [net|Hello|]
-- [net|Hello|] :: NonEmptyText
-- ```
--
-- See `Gyler.Context.*` modules for usage examples.

module Gyler.Data.NonEmptyText.QQ (
    net
) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Data.Text as T (Text, uncons, pack)
import Gyler.Data.NonEmptyText (NonEmptyText(..), new)

-- Helper for quoteExp
parseNonEmpty :: String -> Q (Char, T.Text)
parseNonEmpty s = case T.uncons (T.pack s) of
    Just pair -> pure pair
    Nothing   -> fail "NonEmptyText quasiquoter: empty string is not allowed"

net :: QuasiQuoter
net = QuasiQuoter
  { quoteExp  = \s -> do
      (h, t) <- parseNonEmpty s
      [| new h t |]

  , quotePat  = error "quotePat is not supported for NonEmptyText"
  , quoteType = error "quoteType is not supported for NonEmptyText"
  , quoteDec  = error "quoteDec is not supported for NonEmptyText"
  }
