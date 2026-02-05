{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Gyler.Domain.RuntimeValidated.Template (
    mkRvNonEmptyText
) where

-- Module: Gyler.Domain.RuntimeValidated.Template
--
-- Description: Template for creating RuntimeValidated domain entities.
--
-- All entities are defined in the same way and differ only in the type and set
-- names. This macro creates a standard RuntimeValidated type with the given
-- name. It allows all such definitions to remain concise and makes it easy to
-- maintain and apply changes to the RuntimeValidated structure from a single
-- place.
--
-- This macro is specific to the Gyler domain, which uses only NonEmptyText and
-- HashSet for RuntimeValidated types, so both are hardcoded here.

import Language.Haskell.TH
import Data.Generics (everywhere, mkT)

import Gyler.Data.NonEmptyText (NonEmptyText)

import Gyler.Classes.RuntimeValidated.Internal (RuntimeValidated(..))
import Gyler.Classes.RuntimeValidated.Parser.Raw (HasParsableRaw(..))

import Gyler.Data.ValidContainer.HashSet (HashSet)

import Gyler.Parsers (nonEmptyText, latinAlphaNum)

import Gyler.Domain.RPM.Symbols (rpmChars)

import Text.Megaparsec (some, (<|>), oneOf)

import qualified Data.Text as T (pack)

import Data.Hashable (Hashable)
import Data.Serialize (Serialize)

mkRvNonEmptyText :: String -> String -> Q [Dec]
mkRvNonEmptyText typeNameStr setNameStr =
  let tyName  = mkName typeNameStr
      setName = mkName setNameStr

      decs = [d|
        newtype RV_TYPE = RV_TYPE NonEmptyText
            deriving stock Show
            deriving newtype (Eq, Hashable, Serialize)

        instance RuntimeValidated RV_TYPE where
            type Raw RV_TYPE = NonEmptyText
            mkUnsafe = RV_TYPE
            getRaw (RV_TYPE x) = x

        type RV_SET = HashSet RV_TYPE

        instance HasParsableRaw RV_TYPE where
            rawParser _ = nonEmptyText (T.pack <$> some (latinAlphaNum <|> oneOf rpmChars))
        |]

  in renameByBase "RV_TYPE" tyName . renameByBase "RV_SET" setName <$> decs

-- | Rename every occurrence of a TH Name whose 'nameBase' matches the given string.
-- Uses SYB (everywhere + mkT) to traverse the whole AST.
renameByBase :: String -> Name -> [Dec] -> [Dec]
renameByBase fromBase to =
  everywhere (mkT go)
 where
  go :: Name -> Name
  go n
    | nameBase n == fromBase = to
    | otherwise              = n
