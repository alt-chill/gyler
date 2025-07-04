module Gyler.CachedFile (
     CachedFile
    ,ReadFrom(..)
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

== Overview

A 'CachedFile' stores:

* A file path on disk.
* An in-memory cache of the file contents.
* A maximum age (in seconds) after which the file is considered stale.

== Key Functions

* 'newFile': Creates a cache for a file with a custom staleness threshold.
* 'newFileDefault': Creates a cache with a default threshold of 120 seconds.
* 'readCached': Retrieves the cached content, reading from disk if cache is empty and file is fresh.
* 'writeValue': Writes a value both to the file and the cache.
* 'fetchOrRun': Retrieves content from cache or executes a fallback command to regenerate and cache the file.

== Data Types

* 'CachedFile': The main cache structure.
* 'ReadFrom': Indicates the source of the returned data ('Cache', 'Executable', or 'Error').

== Example

@
cf <- newFileDefault "output.txt"
content <- readCached cf

case content of
  Just val -> putStrLn "Loaded from cache or fresh file"
  Nothing  -> putStrLn "Stale or unreadable file"
@

To fall back to an external command if the cache is empty or stale:

@
(resultSource, content) <- fetchOrRun cf ("my-program", ["--flag"])
@

This will run the command only if no valid cached content is available.
-}
