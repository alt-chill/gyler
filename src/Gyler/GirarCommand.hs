{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms #-}

module Gyler.GirarCommand (
    -- * Interface kind and its singletons
    Interface (..),

    -- * Type family: maps Interface to its config type
    ConfigFor,

    -- * Command GADT
    GirarCommand (..),
    SomeGirarCommand (..),

    -- * Patter synonyms
    pattern ViaGyle,
    pattern ViaGitery,
    pattern ViaGirarWeb,

    toCmd,
) where

import Control.Lens ((^.))
import Data.Text    (Text)

import Gyler.Data.NonEmptyText       (NonEmptyText)
import Gyler.Data.NonEmptyText.QQ    (net)
import qualified Gyler.Data.NonEmptyText as NET

import Gyler.Context (
    CommandsConfig, Profile,
    commandsConfig,
    giterySsh, gyleSsh, girarWeb,
    sshExecutable, remoteUser,
    remoteHost, remotePort,
    SshConfig  (SshConfig),
    CurlConfig (CurlConfig),
    curlExecutable
 )

import Gyler.Utils.Errors (mkErr)
import Gyler.Types        (Cmd)

import Data.Maybe (fromMaybe)

-- ---------------------------------------------------------------------------
-- Interface kind
-- ---------------------------------------------------------------------------

-- | Enumerates the available transport interfaces to Girar.
data Interface
    = Gyle
    | Gitery
    | GirarWeb
    deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Type family: Interface -> Config
-- ---------------------------------------------------------------------------

-- | Maps each 'Interface' to the configuration type it requires.
-- This is the key piece that ties a command to its config at the type level.
type family ConfigFor (i :: Interface) where
    ConfigFor 'Gyle     = SshConfig
    ConfigFor 'Gitery   = SshConfig
    ConfigFor 'GirarWeb = CurlConfig

-- ---------------------------------------------------------------------------
-- Command GADT
-- ---------------------------------------------------------------------------

-- | A Girar command parameterised by the interface it uses.
data GirarCommand (i :: Interface) where
    GyleCommand     :: ![NonEmptyText] -> GirarCommand 'Gyle
    GiteryCommand   :: ![NonEmptyText] -> GirarCommand 'Gitery
    GirarWebCommand :: !NonEmptyText   -> GirarCommand 'GirarWeb

deriving instance Show (GirarCommand i)
deriving instance Eq   (GirarCommand i)

-- Pattern Synonyms

pattern ViaGyle :: [NonEmptyText] -> SomeGirarCommand
pattern ViaGyle args = SomeGirarCommand (GyleCommand args)

pattern ViaGitery :: [NonEmptyText] -> SomeGirarCommand
pattern ViaGitery args = SomeGirarCommand (GiteryCommand args)

pattern ViaGirarWeb :: NonEmptyText -> SomeGirarCommand
pattern ViaGirarWeb ep = SomeGirarCommand (GirarWebCommand ep)

-- ---------------------------------------------------------------------------
-- Existential wrapper
-- ---------------------------------------------------------------------------

data SomeGirarCommand = forall i. SomeGirarCommand (GirarCommand i)

instance Show SomeGirarCommand where
    show (SomeGirarCommand cmd) = "SomeGirarCommand (" <> show cmd <> ")"

-- | Extract the configuration required by a command from 'CommandsConfig'.
selectConfig
    :: GirarCommand i
    -> CommandsConfig
    -> Maybe (ConfigFor i)
selectConfig (GyleCommand _)     cfg = cfg ^. gyleSsh
selectConfig (GiteryCommand _)   cfg = cfg ^. giterySsh
selectConfig (GirarWebCommand _) cfg = cfg ^. girarWeb

-- | Assemble the final executable 'Cmd' from a command and its resolved config.
buildCmd :: GirarCommand i -> ConfigFor i -> Cmd
buildCmd (GyleCommand   args) ssh  = fromSsh  ssh  args
buildCmd (GiteryCommand args) ssh  = fromSsh  ssh  args
buildCmd (GirarWebCommand ep) curl = fromCurl curl ep

-- | Convert a 'SomeGirarCommand' to an executable 'Cmd' using the supplied
-- 'Profile'.
--
-- Fails with 'Left' when the required transport is not configured.
toCmd :: Profile -> SomeGirarCommand -> Either Text Cmd
toCmd profile (SomeGirarCommand cmd) =
    case selectConfig cmd (profile ^. commandsConfig) of
        Nothing  -> Left  $ errMsg cmd "config not available"
        Just cfg -> Right $ buildCmd cmd cfg

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

fromSsh :: SshConfig -> [NonEmptyText] -> Cmd
fromSsh (SshConfig exec user host port key) query =
    let userHost = user <> NET.singleton '@' <> host
        portArg  = [[net|-p|], fromMaybe [net|22|] port]
        keyArg   = maybe [] (\k -> [[net|-i|], k]) key
        args     = portArg ++ keyArg ++ [userHost]
    in (exec, args ++ query)

fromCurl :: CurlConfig -> NonEmptyText -> Cmd
fromCurl (CurlConfig exec address) endpoint =
    (exec, [address <> NET.singleton '/' <> endpoint])

-- | Build an error message that includes the command's constructor name.
errMsg :: GirarCommand i -> Text -> Text
errMsg cmd =
    mkErr $ "toCmd (" <> interfaceName cmd <> ")"
  where
    interfaceName :: GirarCommand i -> Text
    interfaceName (GyleCommand _)     = "GyleCommand"
    interfaceName (GiteryCommand _)   = "GiteryCommand"
    interfaceName (GirarWebCommand _) = "GirarWebCommand"
