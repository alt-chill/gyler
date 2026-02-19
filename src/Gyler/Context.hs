{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Gyler.Context - central gyler execution context.
--
-- See Gyler.GylerM.

module Gyler.Context (
    module Gyler.Profile,
    GylerContext(..),
    defContext,
    girarEnv, profile,
    cacheDir, logger
) where

import Gyler.Profile

import Control.Lens (makeLenses)

import Gyler.GirarEnv (GirarEnv)

import Gyler.Data.NonEmptyText (NonEmptyText)
import Gyler.Data.NonEmptyText.QQ (net)

import Gyler.Logging (HasLogger(..), LogFunc)

import Gyler.Domain.Maintainer (Maintainer)

data GylerContext = GylerContext
    { _girarEnv :: !(Maybe GirarEnv)
    , _cacheDir :: !FilePath

    , _profile  :: !Profile

    , _logger   :: !LogFunc
    }

makeLenses ''GylerContext

defContext :: GylerContext
defContext = GylerContext
    { _girarEnv = Nothing
    , _cacheDir = "/tmp"
    , _profile  = defProfile
    , _logger   = \_ _ -> return ()
    }

instance HasLogger GylerContext where
    getLogger = _logger
