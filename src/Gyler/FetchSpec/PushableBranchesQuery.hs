{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Gyler.FetchSpec.PushableBranchesQuery
-- Description : Fetch spec for listing branches of current girar
--
-- The command associated with this spec is:
--
-- > ssh gyle acl --list

module Gyler.FetchSpec.PushableBranchesQuery (
    PushableBranchesQuery (..)
 ) where

import Gyler.FetchSpec (FetchSpec (..))
import Gyler.GirarCommand (GirarCommand (ViaGyle))

import qualified Gyler.Data.NonEmptyText as NET (lines, fromText)
import Gyler.Data.NonEmptyText.QQ (net)

import Gyler.Classes.RuntimeValidated (mkValidSet)

import Gyler.Domain.PushableBranch (PushableBranchesSet)

import Data.Text.Encoding as DTE (decodeLatin1)

data PushableBranchesQuery = PushableBranchesQuery deriving (Eq, Show)

instance FetchSpec PushableBranchesQuery where
    type Result PushableBranchesQuery = PushableBranchesSet

    command _ = ViaGyle [[net|acl|], [net|--list|]]

    cacheFileName _ = "pushable_branches_set"
    -- It's not often that new branches appear.
    staleAfter _    = 86400

    -- Output of 'ssh gyle acl --list' looks like:
    --   c10f1
    --   c10f2
    --   ...
    --   p9
    --   sisyphus
    -- So we just split output with '\n' as delimiter
    parseResult _ _ input =
          mkValidSet
        . NET.lines
          <$> (NET.fromText . DTE.decodeLatin1 $ input)
