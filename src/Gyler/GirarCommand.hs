{-# LANGUAGE OverloadedStrings #-}

module Gyler.GirarCommand (
    GirarCommand (..),
    toCmd
    ) where

-- | Represents abstraction for executing commands through girar using
-- different ways (Gyle, Gitery, or Curl).
--
-- Used by GirarEntities to return.

import Control.Lens ((^.))
import Data.Text (Text)

import Gyler.GylerM (GylerM)
import Gyler.Context (
    CommandsConfig,
    giteryConfig, gyleConfig, curlConfig,
    sshExecutable, sshArgs, remoteUser,
    remoteHost, remotePort, SshConfig (SshConfig),
    curlExecutable, curlArgs
 )
import Data.Maybe (fromMaybe, maybe)

import Gyler.Types (Cmd)

data GirarCommand
    = ViaGyle   { gyleArgs    :: ![Text] }
    | ViaGitery { gitertyArgs :: ![Text]}
    | ViaCurl   { endpoint    :: !Text }

-- | Convert SSH configuration into executable command format
--
-- Constructs a command tuple (executable, arguments) from:
-- * SSH executable path
-- * Additional SSH arguments
-- * Remote connection credentials (user@host)
-- * Port (defaults to 22 if not specified)
-- * Optional identity file
fromSsh :: SshConfig -> Cmd
fromSsh (SshConfig exec extra user host port key) =
  ( exec
  , extra ++
    [ user <> "@" <> host
    , "-p"
    , fromMaybe "22" port
    ] ++
    maybe [] (\i -> ["-i", i]) key
  )

-- | Convert a GirarCommand to an cmd using configuration from context.
--
-- Parameters:
--   * 'cfg' - Configuration from Gyler.Context
--
-- Examples:
-- >>> toCmd config (ViaGitery ["ls", "/"])
-- ("ssh", ["-p","22","user@host","ls","/"])
--
-- >>> toCmd config (ViaCurl "https://api.example.com")
-- ("curl", ["https://api.example.com"])
toCmd :: CommandsConfig -> GirarCommand -> Cmd
toCmd cfg (ViaGitery args) = fmap (++ args) (fromSsh (cfg ^. giteryConfig))
toCmd cfg (ViaGyle args)   = fmap (++ args) (fromSsh (cfg ^. gyleConfig))

toCmd cfg (ViaCurl target) =
  let useCfg = cfg ^. curlConfig
  in ( useCfg ^. curlExecutable
     , (useCfg ^. curlArgs) ++ [target]
     )
