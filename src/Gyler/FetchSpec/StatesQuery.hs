{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Gyler.FetchSpec.StatesQuery
-- Description : Fetch specification to retrieve all available task states in Girar
--
-- The command associated with this specification is:
--
-- > ssh gyle task ls --help

module Gyler.FetchSpec.StatesQuery (
    StatesQuery (..)
 ) where

import Gyler.FetchSpec (FetchSpec (..))
import Gyler.GirarCommand (GirarCommand (ViaGyle))

import qualified Gyler.Data.NonEmptyText as NET (lines, fromText)
import Gyler.Data.NonEmptyText.QQ (net)

import Gyler.Classes.RuntimeValidated (mkValidSet)

import Gyler.Domain.State (StatesSet)

import Gyler.Parsers (Parser, useParser, lexeme, word, symbol)

import Text.Megaparsec (try, option, manyTill, anySingle)
import Text.Megaparsec.Char (char)

import Data.Text.Encoding as DTE (decodeLatin1)
import Data.Text (Text, toUpper)

import Control.Monad (void)

import Data.Maybe (mapMaybe)

data StatesQuery = StatesQuery deriving (Eq, Show)

instance FetchSpec StatesQuery where
    type Result StatesQuery = StatesSet

    command _ = ViaGyle [[net|task|], [net|ls|], [net|--help|]]

    cacheFileName _ = "states_set"

    -- It's not often that new states appear.
    staleAfter _    = 86400

    -- The output of 'ssh gyle task ls --help' looks like:
    --   task ls - list tasks
    --
    --   Usage: task ls [options]
    --
    --   Options:
    --     ....
    --     ....
    --     ....
    --   Valid task state names are:
    --      awaiting building committing done eperm failing failed new pending postponed swept tested, or ALL.
    --     ...
    --
    -- Therefore, we skip all text before "Valid task state names are" and then
    -- extract the state names. The "ALL" state is ignored.
    --
    -- In Girar, state names are usually written in uppercase, for example:
    --     #394111 TESTED #1 [test-only] sisyphus cabal-vendor.git=1.1.0-alt1
    --
    -- We also convert them to uppercase for consistency.
    parseResult _ _ input = useParser msgParser (decodeLatin1 input)
        where
            msgParser :: Parser StatesSet
            msgParser = do
                void $ manyTill anySingle (symbol "Valid task state names are:")
                repos <- mapMaybe (NET.fromText . toUpper) <$>
                            manyTill word (char ',')
                return (mkValidSet repos)
