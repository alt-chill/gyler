{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Configuration for running external CURL commands.
-- Includes the path to the curl executable and its default arguments.

module Gyler.Context.Curl (
    CurlConfig,
    curlExecutable, curlArgs,
    defCurlConfig
) where

import Control.Lens (makeLenses)
import Data.Text (Text)

data CurlConfig = CurlCommandConfig
    { _curlExecutable :: !Text
    , _curlArgs       :: ![Text]
    } deriving (Show, Eq)

makeLenses ''CurlConfig

defCurlConfig :: CurlConfig
defCurlConfig = CurlCommandConfig
    { _curlExecutable = "curl"
    , _curlArgs = []
    }
