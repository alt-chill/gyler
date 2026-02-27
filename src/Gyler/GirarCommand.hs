{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Gyler.GirarCommand (
    GirarCommand (..),
    toCmd
) where

-- | Represents abstraction for executing commands through girar using
-- different ways (Gyle, Gitery, or GirarWeb interface).

import Control.Lens ((^.))
import Data.Text (Text)
import Gyler.Data.NonEmptyText (NonEmptyText)
import Gyler.Data.NonEmptyText.QQ (net)
import qualified Gyler.Data.NonEmptyText as NET
import Data.Maybe (fromMaybe)

import Gyler.GylerM (GylerM)
import Gyler.Context (
    CommandsConfig, Profile,
    commandsConfig,
    giterySsh, gyleSsh, girarWeb,
    sshExecutable, remoteUser,
    remoteHost, remotePort, SshConfig (SshConfig),
    curlExecutable, CurlConfig (CurlConfig)
 )
import Gyler.Utils.Errors (mkErr)
import Gyler.Types (Cmd)

-- | Abstraction of Girar interaction.
data GirarCommand
    = ViaGyle     { gyleArgs    :: ![NonEmptyText] }
    | ViaGitery   { giteryArgs  :: ![NonEmptyText]}
    | ViaGirarWeb { endpoint    :: !NonEmptyText }
    deriving (Show, Eq)

-- | Used for logging on errors (constructor only).
typeName :: GirarCommand -> Text
typeName (ViaGyle _)     = "ViaGyle"
typeName (ViaGitery _)   = "ViaGitery"
typeName (ViaGirarWeb _) = "ViaGirarWeb"

-- | Convert SSH configuration into executable command format.
fromSsh :: Maybe SshConfig -> [NonEmptyText] -> Either Text Cmd
fromSsh (Just (SshConfig exec user host port key)) query =
    let userHost = user <> [net|@|] <> host
        portArg  = [[net|-p|], fromMaybe [net|22|] port]
        keyArg   = maybe [] (\k -> [[net|-i|], k]) key
        args     = portArg ++ keyArg ++ [userHost]
    in Right (exec, args ++ query)
fromSsh Nothing _ = Left "SshConfig is not available"

-- | Convert curl configuration into executable command format.
fromCurl :: Maybe CurlConfig -> NonEmptyText -> Either Text Cmd
fromCurl (Just (CurlConfig exec address)) endpoint =
    Right (exec, [address <> NET.singleton '/' <> endpoint] )
fromCurl Nothing _ = Left "CurlConfig is not available"

-- | Convert a GirarCommand to an executable command using config context.
toCmd :: Profile -> GirarCommand -> Either Text Cmd
toCmd profile cmd =
    let cfg = profile ^. commandsConfig in
    case getBaseCmd cfg cmd of
        Left err    -> Left $ errMsg err
        r@(Right _) -> r
  where
    getBaseCmd :: CommandsConfig -> GirarCommand -> Either Text Cmd
    getBaseCmd cfg' = \case
        ViaGyle query        -> fromSsh  (cfg' ^. gyleSsh)   query
        ViaGitery query      -> fromSsh  (cfg' ^. giterySsh) query
        ViaGirarWeb endpoint -> fromCurl (cfg' ^. girarWeb)  endpoint

    errMsg = mkErr $ "toCmd (" <> typeName cmd <> ")"
