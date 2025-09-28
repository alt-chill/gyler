{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Gyler.FetchSpec.Maintainers
-- Description : All registered maintainers from the gitery server.
--
-- The command associated with this entity is:
--
-- > ssh gitery ls /people

module Gyler.FetchSpec.MaintainersQuery (
    MaintainersQuery (..)
) where

import Gyler.FetchSpec (FetchSpec (..))
import Gyler.GirarCommand (GirarCommand (ViaGitery))

import Gyler.Utils.List   (safeLast)
import Gyler.Utils.Maybe  (maybeToRight)
import Gyler.Utils.Errors (mkErr)

import Gyler.Domain.Maintainer (MaintainersSet)

import Gyler.Data.NonEmptyText as NET (fromText, lines, words)
import Gyler.Data.NonEmptyText.QQ (net)

import Gyler.Classes.RuntimeValidated (mkValidSet)

import Gyler.Data.ValidContainer.HashSet (HashSet)

import Data.Text.Encoding as DTE (decodeLatin1)
import Data.Text (pack)

import Data.Maybe (mapMaybe)

data MaintainersQuery = MaintainersQuery deriving (Show, Eq)

instance FetchSpec MaintainersQuery where
    type Result MaintainersQuery = MaintainersSet

    command _ = ViaGitery [[net|ls|], [net|/people|]]

    cacheFileName _ = "maintainers_set"

    -- It's not often that new maintainers appear.
    staleAfter _    = 86400

    -- Output of 'ssh gitery ls /people' looks like:
    --  total 2240
    --  drwxr-xr-x 6 4096 Jan 29  2018 aas
    --  drwxr-xr-x 6 4096 Dec 16  2007 ab
    --  drwxr-xr-x 6 4096 Sep  7  2006 abr
    --  drwxr-xr-x 6 4096 Sep  7  2006 abulava
    --  drwxr-xr-x 6 4096 Mar 28  2018 ac1d
    --  ...
    --
    -- Parser drops the first line (e.g., @"total 2240"@), then for each line
    -- splits it into words and takes the last word — assumed to be a maintainer username.
    --
    -- Result is : ["aas", "ab", "abr" ... ]
    --
    -- It is wrapped as a MaintainersSet using `mkValidSet`.
    --
    -- Since Maintainer is a RuntimeValidated type, we cannot create a set of
    -- maintainers in the usual way.
    --
    -- At the final step, the HashSet is wrapped with a newtype.
    parseResult _ _ input =
          mkValidSet
        . mapMaybe (safeLast . NET.words)
        . drop 1
        . NET.lines
          <$> (maybeToRight (errMsg "Empty input") . NET.fromText
                                                   . DTE.decodeLatin1 $ input)
        where
            errMsg = mkErr $ "parseResult (" <> (pack . show) MaintainersQuery <> ")"
