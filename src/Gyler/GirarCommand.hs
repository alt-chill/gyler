{-# LANGUAGE OverloadedStrings #-}

module Gyler.GirarCommand (
    GirarCommand (..),
    toCmd
) where

import Control.Lens ((^.))
import Data.Maybe       (fromMaybe, maybe)
import Data.Semigroup   ((<>))

import Gyler.Data.NonEmptyText
       ( NonEmptyText
       , singleton
       )

import Gyler.Context
       ( CommandsConfig
       , SshConfig (SshConfig)
       , giteryConfig, gyleConfig, curlConfig
       , sshExecutable, sshArgs, remoteUser
       , remoteHost, remotePort
       , curlExecutable, curlArgs
       )

import Gyler.Types (Cmd)

-- | Abstraction over the three ways we can interact with girar
data GirarCommand
    = ViaGyle   { gyleArgs    :: ![NonEmptyText] }
    | ViaGitery { gitertyArgs :: ![NonEmptyText] }
    | ViaCurl   { endpoint    :: !NonEmptyText }

-- | Build the user@host segment
at :: NonEmptyText -> NonEmptyText -> NonEmptyText
at usr host = usr <> singleton '@' <> host

-- | Convert an 'SshConfig' into an executable/name pair.
--   The resulting list of arguments is also non‑empty.
fromSsh :: SshConfig -> Cmd
fromSsh (SshConfig exec extra user host port key) =
    ( exec
    ,    extra
      ++ [ user `at` host
         , "-p"
         , fromMaybe "22" port
         ]
      ++ maybe [] (\i -> ["-i", i]) key
    )

-- | Translate a 'GirarCommand' into the concrete command ready for @process@.
--   Uses the executable/argument defaults from the provided 'CommandsConfig'.
--
-- >>> toCmd cfg (ViaCurl "https://git.altlinux.org")
-- ("curl", ["https://git.altlinux.org"])
--
-- NOTE: This function never returns an empty executable or argument segment.
toCmd :: CommandsConfig -> GirarCommand -> Cmd
toCmd cfg (ViaGitery args) = fmap (++ args) (fromSsh (cfg ^. giteryConfig))
toCmd cfg (ViaGyle   args) = fmap (++ args) (fromSsh (cfg ^. gyleConfig))

toCmd cfg (ViaCurl target) =
  let cCfg = cfg ^. curlConfig
  in ( cCfg ^. curlExecutable
     , (cCfg ^. curlArgs) ++ [target]
     )
