{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : Gyler.GirarEntity.Maintainers
-- Description : All registered maintainers from the gitery server.
--
-- The command associated with this entity is:
--
-- > ssh gitery ls /people
--
-- The result is cached using the filename @"maintaines"@ and has a
-- staleness threshold of 86400 seconds (24 hours).
--
-- No additional processing or context is required.

module Gyler.GirarEntity.Maintainers (
    Maintainers (..)
) where

import Gyler.GirarEntity (GirarEntity (..))
import Gyler.GirarCommand (GirarCommand (ViaGitery))

import qualified Gyler.Data.NonEmptyText as NET (lines, words)
import Gyler.Data.NonEmptyText.QQ (net)

import Data.Maybe (mapMaybe)

import Gyler.Utils.List (safeLast)

data Maintainers = Maintainers deriving (Show, Eq)

instance GirarEntity Maintainers where
    getGirarCommand _ = ViaGitery [[net|ls|], [net|/people|]]

    getCachedFilename _ = "maintaines"

    -- It's not often that new branches appear.
    getStaleAge _       = 86400

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
    parseValue _ _ = mapMaybe (safeLast . NET.words) .  drop 1 . NET.lines
