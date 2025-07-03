module Gyler.CachedFile (
    CachedFile (..)
    ,newFile
    ,newFileDefault
    ,readCached
    ,writeValue
    ,fetchOrRun
) where

import Gyler.CachedFile.Internal

{-
Module      : Gyler.CachedFile
Description : Handle cached file data with time-based invalidation.

This module defines a `CachedFile` abstraction that manages a file's
cached content in memory, automatically detecting it up when the cache
becomes stale based on a specified time threshold (`NominalDiffTime`).

* 'CachedFile': Configuration type (path, cache ref, freshness threshold)
* 'newFile': Creates a cache with custom staleness threshold (seconds)
* 'newFileDefault': Creates cache with default 120s staleness threshold
* 'readCached': Safely retrieves cached content:
  - Uses cached version when available
  - Reads from disk if cache is empty but file is fresh
  - Returns 'Nothing' for unreadable/stale files
* 'writeValue': Writes value to cache and into file
* 'fetchOrRun': Attempts to retrieve a cached value.
                Runs external command if cache is not available.

== Example Usage

@
cf <- newFileDefault "cached_output.txt"
content <- readCached cf

case content of
    Just val -> print "Loaded from cache/fresh disk read"
    Nothing  -> print "File stale or missing! Handle accordingly..."
@
-}
