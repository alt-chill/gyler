{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The main application context passed throughout Gyler.
--
-- Provides default setup via 'defContext'.

module Gyler.Context.Gyler (
    GylerContext(..),
    defContext,
    commandsConfig, girarEnv, altUser,
    cacheDir
) where

import Control.Lens (makeLenses)
import Data.Text (Text)

import Gyler.Context.Commands
import Gyler.GirarEnv (GirarEnv)

data GylerContext = GylerContext
    { _commandsConfig :: !CommandsConfig
    , _girarEnv       :: !(Maybe GirarEnv)
    , _cacheDir       :: !FilePath
    , _altUser        :: !Text
    } deriving (Show, Eq)

makeLenses ''GylerContext

defContext :: GylerContext
defContext = GylerContext
    { _commandsConfig = defCommandsConfig
    , _girarEnv = Nothing
    , _cacheDir = "/tmp"
    , _altUser = ""
    }
