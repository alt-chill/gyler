module Gyler.GirarEntity (GirarEntity (..), completeWith) where

-- |
-- Module      : Gyler.GirarEntity
-- Description : Abstraction over Girar entities
--
-- This module defines the 'GirarEntity' typeclass, which abstracts over
-- different Girar entities such as tasks, repositories, maintainers,
-- and ACL groups. The 'GirarEntity' typeclass is intended to be used for
-- Gyler autocompletions.
--
-- === Caching
--
-- Data is cached on disk via 'Gyler.CachedFile'.
--
-- The 'getCachedFilename' and 'getStaleAge' methods define how long an entity's
-- output remains valid before it needs to be refreshed.
--
-- If the cache is stale, the Girar command for the entity is executed to update it.
--
-- Cached files are stored in the cacheDir retrieved from 'Gyler.Context'.
--
-- === Parsing
--
-- The 'parseValue' function parses the raw command output into the list of 'NonEmptyText'
-- values. Optionally a 'GirarEnv' is used for context-sensitive parsing.
--
-- See also: 'Gyler.GirarEnv', 'Gyler.Context', 'Gyler.CachedFile'

import Gyler.Data.NonEmptyText (NonEmptyText, fromText)

import Gyler.GylerM (GylerM)
import Gyler.GirarCommand (GirarCommand, toCmd)
import Gyler.GirarEnv (GirarEnv)
import Gyler.Context.Gyler (girarEnv, commandsConfig, cacheDir)

import qualified Gyler.CachedFile as CF (CachedFile, newFile, fetchOrRun)

import Control.Monad.Reader (Reader, liftIO)
import Control.Lens (view, (^.))

import System.FilePath ((</>))

class GirarEntity e where
    -- | Specifies the Girar command used to fetch the entity's data.
    --
    -- This command will be executed only if no fresh cached data is available
    getGirarCommand  :: e -> GirarCommand

    -- | Returns the filename (relative to cache directory from GylerContext)
    --
    -- used to cache the entity's output.
    getCachedFilename :: e -> FilePath

    -- | Specifies how long (in seconds) the cached data remains valid.
    --
    -- After this duration, the cache is considered stale and the command
    -- will be re-executed on the next request.
    getStaleAge       :: e -> Integer -- in seconds

    -- | Parses the raw output of the Girar command.
    --
    -- Returns a list of parsed entries or an empty list on failure.
    parseValue        :: e -> Maybe GirarEnv -> NonEmptyText -> [NonEmptyText]


-- | Constructs a 'CachedFile' for a given Girar entity.
--
-- This helper used by 'getData'.
getCachedFile :: GirarEntity e => e -> GylerM CF.CachedFile
getCachedFile ent = do
    let filename = getCachedFilename ent
        age = fromInteger $ getStaleAge ent
    dir <- view cacheDir
    liftIO $ CF.newFile (dir </> filename) age

-- | Fetches and parses data for a given Girar entity
--
-- The results are used for Gyler CLI autocompletion.
--
-- ==== Note
-- The parsing step may optionally use a 'GirarEnv' from the
-- current 'Gyler.Context', depending on the entity.
--
-- Returns an empty list if command execution or parsing fails,
-- to avoid runtime checks and errors at higher abstraction levels.
completeWith :: GirarEntity e => e -> GylerM [NonEmptyText]
completeWith ent = do
    env <- view girarEnv
    cfg <- view commandsConfig

    let exec = toCmd cfg $ getGirarCommand ent

    cache      <- getCachedFile ent
    (_, output)   <- liftIO $ CF.fetchOrRun cache exec

    case fromText output of
        Just raw -> return $ parseValue ent env raw
        Nothing  -> return []
