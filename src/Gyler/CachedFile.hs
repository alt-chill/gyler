module Gyler.CachedFile (
    CachedFile (..),
    newFile,
    newFileDefault,
    getValue,
    updateFile,
) where

import Gyler.CachedFile.Internal

{-
Module      : Gyler.CachedFile
Description : Handle cached file data with time-based invalidation.

This module defines a `CachedFile` abstraction that manages a file's
cached content in memory, automatically updating it when the cache
becomes stale based on a specified time threshold (`NominalDiffTime`).

* `CachedFile` – A structure representing the cache state, file path, time-to-live, and update command.
* `newFile` – Creates a new `CachedFile` with a user-defined freshness duration.
* `newFileDefault` – Creates a new `CachedFile` with a default freshness duration.
* `getValue` – Retrieves the current cached content, updating from file or command if necessary.
* `updateFile` - Runs the external command to regenerate the file content.

The update mechanism relies on running a command whose output
becomes the new file content when the cache is stale or uninitialized.

== Example Usage

@
let cmd = ("ls", [])
cf <- newFileDefault "cached_output.txt" cmd
content <- getValue cf
@
-}
