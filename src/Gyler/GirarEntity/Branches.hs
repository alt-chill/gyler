{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Gyler.GirarEntity.Branch
-- Description : Girar entity for listing branches of current girar
--
-- The command associated with this entity is:
--
-- > ssh gyle acl --list
--
-- The result is cached using the filename @"branches"@ and has a
-- staleness threshold of 86400 seconds (24 hours).
-- No additional processing or context is required.

module Gyler.GirarEntity.Branches (
    Branches (..)
 ) where

import Gyler.GirarEntity (GirarEntity (..))
import Gyler.GirarCommand (GirarCommand (ViaGyle))

import qualified Data.Text as T (lines)

data Branches = Branches deriving (Eq, Show)

instance GirarEntity Branches where
    getGirarCommand _ = ViaGyle ["acl", "--list"]

    getCachedFilename _ = "branches"
    -- It's not often that new branches appear.
    getStaleAge _       = 86400

    -- Output of 'ssh gyle acl --list' looks like:
    --   c10f1
    --   c10f2
    --   ...
    --   p9
    --   sisyphus
    -- So we just split output with '\n' as delimiter
    parseValue _ _ = T.lines
