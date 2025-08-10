{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Configuration for running external CURL commands.
-- Includes the path to the curl executable and its default arguments.

module Gyler.Context.Curl (
    CurlConfig (CurlConfig),
    curlExecutable, curlArgs,
    defCurlConfig
) where

import Control.Lens (makeLenses)
import Gyler.Data.NonEmptyText.Unsafe (NonEmptyText)

data CurlConfig = CurlConfig
    { _curlExecutable :: !NonEmptyText
    , _curlArgs       :: ![NonEmptyText]
    } deriving (Show, Eq)

makeLenses ''CurlConfig

defCurlConfig :: CurlConfig
defCurlConfig = CurlConfig
    { _curlExecutable = "curl"
    , _curlArgs = []
    }
