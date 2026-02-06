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
    CommandsConfig,
    giterySsh, gyleSsh, girarWeb,
    sshExecutable, sshArgs, remoteUser,
    remoteHost, remotePort, SshConfig (SshConfig),
    curlExecutable, curlArgs, CurlConfig (CurlConfig)
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

-- | Return argument list.
argsOf :: GirarCommand -> [NonEmptyText]
argsOf (ViaGyle args)    = args
argsOf (ViaGitery args)  = args
argsOf (ViaGirarWeb arg) = [arg]

-- | Convert SSH configuration into executable command format.
fromSsh :: Maybe SshConfig -> Either Text Cmd
fromSsh (Just (SshConfig exec extra user host port key)) =
    let userHost = user <> [net|@|] <> host
        portArg  = [[net|-p|], fromMaybe [net|22|] port]
        keyArg   = maybe [] (\k -> [[net|-i|], k]) key
        args     = extra ++ portArg ++ keyArg ++ [userHost]
    in Right (exec, args)
fromSsh Nothing = Left "SshConfig is not available"

-- | Convert curl configuration into executable command format.
fromCurl :: Maybe CurlConfig -> Either Text Cmd
fromCurl (Just (CurlConfig exec args)) =
    Right (exec, args)
fromCurl Nothing = Left "CurlConfig is not available"

-- | Convert a GirarCommand to an executable command using config context.
toCmd :: CommandsConfig -> GirarCommand -> Either Text Cmd
toCmd cfg cmd =
    case getBaseCmd cfg cmd of
        Left err           -> Left $ errMsg err
        Right (exec, args) -> Right (exec, args ++ argsOf cmd)
  where
    getBaseCmd :: CommandsConfig -> GirarCommand -> Either Text Cmd
    getBaseCmd cfg' = \case
        ViaGyle _       -> fromSsh  (cfg' ^. gyleSsh)
        ViaGitery _     -> fromSsh  (cfg' ^. giterySsh)
        ViaGirarWeb _   -> fromCurl (cfg' ^. girarWeb)

    errMsg = mkErr $ "toCmd (" <> typeName cmd <> ")"
