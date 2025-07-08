module Gyler.GirarEntity (GirarEntity (..), getData) where

-- |
-- Module      : Gyler.GirarEntity
-- Description : Abstraction over Girar entities
--
-- This module defines the 'GirarEntity' typeclass, which abstracts over
-- different Girar entities such as tasks, repositories, maintainers,
-- and ACL groups.
--
-- Entities that implement this typeclass provide:
--
-- 1. A command used to retrieve the data ('getGirarCommand')
-- 2. A unique filename for caching the output ('getCachedFilename')
-- 3. A staleness period in seconds for cache invalidation ('getStaleAge')
-- 4. A parser that processes the command output, optionally using 'GirarEnv' ('parseValue')
--
-- === Caching
--
-- Data is cached on disk and optionally in memory via 'Gyler.CachedFile'.
-- The 'getCachedFilename' and 'getStaleAge' methods define how long an entity's
-- output remains valid before it needs to be refreshed.
--
-- === Parsing
--
-- The 'parseValue' function receives the raw command output and an optional 'GirarEnv',
-- which may contain context such as known branches, states, or maintainers. This
-- allows parsing to be context-sensitive when needed.
--
-- === Using
--
-- To load an entity:
--
-- > instance GirarEntity MyEntity where ...
--
-- You can then retrieve its cached or freshly generated data using utilities
-- from the 'Gyler.CachedFile' module in conjunction with this typeclass.
--
-- See also: 'Gyler.GirarEnv', 'Gyler.Context', 'Gyler.CachedFile'

import Data.Text (Text)

import Gyler.GylerM (GylerM)
import Gyler.GirarCommand (GirarCommand, toCmd)
import Gyler.GirarEnv (GirarEnv)
import Gyler.Context.Gyler (girarEnv, commandsConfig, cacheDir)

import qualified Gyler.CachedFile as CF (CachedFile, newFile, fetchOrRun)

import Control.Monad.Reader (Reader, liftIO)
import Control.Lens (view, (^.))

import System.FilePath ((</>))

-- | Typeclass for Girar entity descriptors.
--
-- A 'GirarEntity' defines how a specific kind of entity (e.g., branches,
-- maintainers) is queried, cached, and parsed.
--
-- This abstraction allows declarative specification of fetchable entities
-- with minimal boilerplate.
class GirarEntity e where
    -- | Specifies the Girar command used to fetch the entity's data.
    --
    -- This command will be executed only if no fresh cached data is available
    getGirarCommand  :: e -> GirarCommand

    -- | Returns the filename (relative to cache directory from GylerContext)
    -- used to cache the entity's output.
    getCachedFilename :: e -> FilePath

    -- | Specifies how long (in seconds) the cached data remains valid.
    --
    -- After this duration, the cache is considered stale and the command
    -- will be re-executed on the next request.
    getStaleAge       :: e -> Integer -- in seconds

    -- | Parses the raw output of the Girar command into structured values.
    --
    -- Returns a list of parsed entries, or an empty list on failure.
    parseValue        :: e -> Maybe GirarEnv -> Text -> [Text]


{- Not type-class functions -}

-- | Constructs a 'CachedFile' for a given Girar entity.
--
-- This sets up caching logic using the filename and staleness period
-- defined by the entity’s 'GirarEntity' instance.
--
-- The cache file is created under the global cache directory, which is
-- provided by the current 'Gyler.Context'.
--
-- This is a low-level helper, used by 'getData'.
getCachedFile :: GirarEntity e => e -> GylerM CF.CachedFile
getCachedFile ent = do
    let filename = getCachedFilename ent
        age = fromInteger $ getStaleAge ent
    dir <- view cacheDir
    liftIO $ CF.newFile (dir </> filename) age

-- | Fetches and parses data for a given Girar entity.
--
-- This function checks the cached file associated with the entity.
-- If the data is fresh (i.e., not older than 'getStaleAge'), it is
-- loaded from the cache. Otherwise, the 'getGirarCommand' is executed
-- to regenerate the data, and the result is cached.
--
-- Once the raw output is obtained (either from cache or command),
-- 'parseValue' is called to transform it into a list of structured
-- values.
--
-- ==== Note
-- The parsing step may optionally rely on a 'GirarEnv' from the
-- current 'Gyler.Context', depending on the entity.
--
-- Returns an empty list if command execution or parsing fails.
getData :: GirarEntity e => e -> GylerM [Text]
getData ent = do
    env <- view girarEnv
    cfg <- view commandsConfig

    let exec = toCmd cfg $ getGirarCommand ent

    cache      <- getCachedFile ent
    (_, raw)   <- liftIO $ CF.fetchOrRun cache exec

    return $ parseValue ent env raw
