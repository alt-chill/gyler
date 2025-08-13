{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Configuration for running external CURL commands.
-- Includes the path to the curl executable and its default arguments.

module Gyler.Context.Curl (
    CurlConfig (CurlConfig),
    curlExecutable, curlArgs,
    defCurlConfig
) where

import Control.Lens (makeLenses)

import Gyler.Data.NonEmptyText (NonEmptyText)
import Gyler.Data.NonEmptyText.QQ (net)

data CurlConfig = CurlConfig
    { _curlExecutable :: !NonEmptyText
    , _curlArgs       :: ![NonEmptyText]
    } deriving (Show, Eq)

makeLenses ''CurlConfig

defCurlConfig :: CurlConfig
defCurlConfig = CurlConfig
    { _curlExecutable = [net|curl|]
    , _curlArgs = []
    }
