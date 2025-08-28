{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Gyler.FetchSpec (
    FetchSpec(..),
    fetch
) where

-- | Module: Gyler.FetchSpec
--
-- === Description ===
-- The `FetchSpec` module defines an interface for specifying how
-- certain data should be fetched from Girar.
--
-- For example, to fetch a set of tasks, you would define a new data type
-- with the appropriate fetching parameters and then provide a
-- `FetchSpec` instance for it.
--
-- The type of data to be fetched must be specified as a type family member.
--
-- ```
-- newtype TasksSet = HashSet TaskNum
--     deriving newtype Serialize
--
-- data TasksQuery = FetchTasks {
--     ...
-- }
--
-- instance FetchSpec TasksQuery where
--     type Result TaskQuery = TasksSet
--     ...
-- ```
--
-- In this case, `parseResult` should return a `TasksSet` value.
--
-- WARNING: `Result` is injective!
-- This means multiple fetch descriptions cannot be defined
-- for the same data type.
--
-- Use newtype wrappers if neccessary.
--
-- === Caching ===
-- To avoid expensive and unnecessary external calls to Girar, fetched data
-- is serialized and cached using the `Gyler.CachedFile` module.
-- Therefore, `Result` must implement a `Serialize` instance.
--
-- Caching behavior is configured through the typeclass functions
-- `cacheFileName` and `staleAfter`.
--
-- Cached files are stored in `cacheDir`, which is obtained from `Gyler.Context`.
--
-- === Parsing ===
-- Parsing may require additional information from `GirarEnv`, especially
-- when working with RuntimeValidated types such as `Maintainer`, `Branch`, etc.

import Gyler.Data.NonEmptyText (NonEmptyText, fromText)

import Gyler.GylerM (GylerM)
import Gyler.GirarCommand (GirarCommand, toCmd)
import Gyler.GirarEnv (GirarEnv)
import Gyler.Context.Gyler (girarEnv, commandsConfig, cacheDir)
import Gyler.Utils.Maybe (rightToMaybe)

import qualified Gyler.CachedFile as CF (CachedFile, newFile, readData, writeValue)

import Gyler.Types (Cmd)

import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString, toStrict)

import qualified Gyler.Data.NonEmptyText as NET (unpack)

import Control.Exception (SomeException, try)

import System.Process.Typed (proc, readProcessStdout_)

import Control.Monad.Reader (liftIO, lift)
import Control.Lens (view)

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Applicative ((<|>))

import System.FilePath ((</>))

import Data.Serialize (Serialize, decode, encode)

class (Serialize (Result e)) => FetchSpec e where
    type Result e = r | r -> e

    command       :: e -> GirarCommand
    cacheFileName :: e -> FilePath
    staleAfter    :: e -> Integer
    parseResult   :: e -> Maybe GirarEnv -> BS.ByteString -> Maybe (Result e)

-- | Constructs a 'CachedFile' for a given Girar entity.
--
-- This helper used by 'getData'.
prepareCacheFile :: FetchSpec e => e -> GylerM CF.CachedFile
prepareCacheFile ent = do
    let filename = cacheFileName ent
        age = fromInteger $ staleAfter ent
    dir <- view cacheDir
    liftIO $ CF.newFile (dir </> filename) age

-- | Try to fetch data from the description
--
-- Order of checks:
--  1. Fail if updating the command is not possible.
--
--     This check comes first because `toCmd` returns an error only when no method
--     of interacting with Girar (SSH interfaces for Gitery and Gyle, or HTTP access
--     to the web interface) is configured.
--
--     In such cases, fetching should always fail. Returning a cached value when
--     the command is not configured would be non-obvious behavior.
--
--  2. Attempt to read the value from the cache. If a cache entry exists and can
--     be decoded, return it. If the cache is missing or decoding fails, proceed
--     to step 3.
--
--  3. Try to run the Girar command and parse its output.
--
--     If both fetching and parsing succeed, update the cache with the new value
--     and return it.
--
--     If anything goes wrong at this step, the function returns Nothing.
--     fetch :: FetchSpec e => e -> GylerM (Maybe (Result e))
fetch :: FetchSpec e => e -> GylerM (Maybe (Result e))
fetch ent = do
    env <- view girarEnv
    cfg <- view commandsConfig
    case toCmd cfg (command ent) of
        Left _    -> pure Nothing
        Right cmd -> runMaybeT $ do
            file <- lift $ prepareCacheFile ent
            readCache file <|> fetchExternal ent file env cmd
  where
    readCache :: FetchSpec e => CF.CachedFile -> MaybeT GylerM (Result e)
    readCache file = MaybeT $ do
        stored <- liftIO $ CF.readData file
        pure $ stored >>= rightToMaybe . decode

    fetchExternal :: FetchSpec e => e -> CF.CachedFile -> Maybe GirarEnv -> Cmd -> MaybeT GylerM (Result e)
    fetchExternal ent file env cmd = do
        bs  <- MaybeT . lift $ tryRunProcess cmd
        val <- MaybeT . pure $ parseResult ent env bs
        liftIO $ CF.writeValue file (encode val)
        pure val

    tryRunProcess :: Cmd -> IO (Maybe BS.ByteString)
    tryRunProcess (exec, args) = do
        let process = proc (NET.unpack exec) (map NET.unpack args)
        result <- try (readProcessStdout_ process) :: IO (Either SomeException BL.ByteString)
        pure $ either (const Nothing) (Just . BL.toStrict) result

