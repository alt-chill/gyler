{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

module Gyler.FetchSpec.TaskIDsQuery (
    TaskIDsQuery(..)
) where

import Gyler.FetchSpec (FetchSpec (..))
import Gyler.GirarCommand (GirarCommand (ViaGyle))

import Gyler.Domain.Maintainer (Maintainer)
import Gyler.Domain.State      (State)
import Gyler.Domain.Branch     (Branch)

import Gyler.Domain.Task (Task(..), TaskView(IDOnly), pattern IDOnlyTask)

import Gyler.Data.NonEmptyText        (NonEmptyText)
import Gyler.Data.NonEmptyText.QQ     (net)

import Gyler.Classes.IsNonEmptyText (IsNonEmptyText(..))

import Gyler.Parsers (Parser, useParser)

import qualified Data.Text.Encoding as DTE (decodeLatin1)

import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec (many, (<|>), eof, optional, takeWhileP)

import Data.List (foldl')

import Data.Hashable (Hashable(..))
import GHC.Generics  (Generic)

import Numeric (showHex)

-- Mandatory filter:
-- + By maintainer
--
-- Optional filters:
-- + By state
-- + By repository
--
-- Any optional filters not provided are treated as the default
-- parameters of `gyle task ls`.
-- See `gyle task ls --help` for details.
data TaskIDsQuery = TaskIDsQuery
    { targetMaintainer :: !Maintainer
    , targetStates     :: ![State]
    , targetRepos      :: ![Branch]
    } deriving (Eq, Show, Generic)

instance Hashable TaskIDsQuery

argsOf :: TaskIDsQuery -> [NonEmptyText]
argsOf query =
        [    [net|task|],   [net|ls|],
             [net|--user|], toNonEmptyText (targetMaintainer query)
        ] ++ renderFlag [net|--state|] (targetStates query)
          ++ renderFlag [net|--repo|]  (targetRepos  query)
    where
        renderFlag :: IsNonEmptyText f => NonEmptyText -> [f] -> [NonEmptyText]
        renderFlag _     [] = []
        renderFlag label xs = [label, toCsvArg (toNonEmptyText <$> xs)]

        -- The separator is also added at the beginning of the resulting string,
        -- which guarantees that it is non-empty.
        toCsvArg :: [NonEmptyText] -> NonEmptyText
        toCsvArg = foldl' (<>) [net|,|]

instance FetchSpec TaskIDsQuery where
    type Result TaskIDsQuery = [Task 'IDOnly]

    command query = ViaGyle (argsOf query)

    -- Use a hash to store queries with different targets in separate files
    cacheFileName query = "task_ids_" <> showHex (fromIntegral $ hashWithSalt 42 query :: Word) ""

    staleAfter _ = 120

    -- The output of `gyle task ls` looks like:
    --   #405173 TESTED #3 [test-only] sisyphus util-linux.git=2.39.4-alt1
    --   #396361 NEW # [test-only] sisyphus
    --   #394113 TESTED # [test-only] sisyphus ...
    --   ...
    --
    -- The goal of `parseResult` is to produce a list of all task IDs.
    --
    -- Parser then drops the leading '#' character to obtain number string,
    -- which is parsed using Megaparsec’s `decimal` parser.
    --
    -- The `decimal` parser accepts only unsigned numbers, so it is completely safe
    -- to use the `Natural` type here.
    parseResult _ _ input =
        useParser pTaskIds (DTE.decodeLatin1 input)
      where
        pTaskIds :: Parser [Task 'IDOnly]
        pTaskIds = many (pTaskLine <* optional newline) <* eof

        pTaskLine :: Parser (Task 'IDOnly)
        pTaskLine = do
            _   <- char '#'
            tid <- decimal
            _   <- takeWhileP (Just "rest of line") (/= '\n')
            pure $ IDOnlyTask tid
