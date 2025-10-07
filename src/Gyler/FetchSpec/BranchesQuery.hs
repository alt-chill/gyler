{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Gyler.FetchSpec.BranchesQuery
-- Description : Fetch spec for listing branches of current girar
--
-- The command associated with this spec is:
--
-- > ssh gyle task ls --help

module Gyler.FetchSpec.BranchesQuery (
    BranchesQuery (..)
 ) where

import Gyler.FetchSpec (FetchSpec (..))
import Gyler.GirarCommand (GirarCommand (ViaGyle))

import qualified Gyler.Data.NonEmptyText as NET (lines, fromText)
import Gyler.Data.NonEmptyText.QQ (net)

import Gyler.Classes.RuntimeValidated (mkValidSet)

import Gyler.Domain.Branch (BranchesSet)

import Gyler.Parsers (Parser, useParser, lexeme, word, symbol)

import Text.Megaparsec (try, option, manyTill, anySingle)
import Text.Megaparsec.Char (char)

import Data.Text.Encoding as DTE (decodeLatin1)
import Data.Text (Text)

import Data.List (foldl')

import Control.Monad (void)

import Data.Maybe (mapMaybe)

data BranchesQuery = BranchesQuery deriving (Eq, Show)

instance FetchSpec BranchesQuery where
    type Result BranchesQuery = BranchesSet

    command _ = ViaGyle [[net|task|], [net|ls|], [net|--help|]]

    cacheFileName _ = "branches_set"
    -- It's not often that new branches appear.
    staleAfter _    = 86400

    -- Output of 'ssh gyle task ls --help' looks like:
    --   task ls - list tasks
    --
    --   Usage: task ls [options]
    --
    --   Options:
    --     ....
    --
    --   Valid repository names are:
    --     4.0 4.1 5.0 5.1 c10f1 c10f2 c6 c7 c7.1 c8 c8.1 c9f1 c9f2 c9m1 c9m2 icarus p10 p11 p5 p6 p7 p8 p9 sisyphus t6 t7, or ALL.
    --
    -- So we have to skip whole text before 'Valid repository names are' and then
    -- pick up names. 'ALL' is skipped.
    parseResult _ _ input = useParser msgParser (decodeLatin1 input)
        where
            repoParser :: Parser Text
            repoParser = lexeme $
                         (<>) <$>
                         word <*>
                         option "" (try ((<>) <$> symbol "." <*> word))

            msgParser :: Parser BranchesSet
            msgParser = do
                void $ manyTill anySingle (symbol "Valid repository names are:")
                repos <- mapMaybe NET.fromText <$>
                            manyTill repoParser (char ',')
                return (mkValidSet repos)
