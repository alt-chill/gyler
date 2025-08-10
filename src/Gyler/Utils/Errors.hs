{-# Language OverloadedStrings #-}

module Gyler.Utils.Errors (mkErr) where

import Data.Text (Text)

mkErr :: Text -> Text -> Text
mkErr who msg = who <> ": " <> msg
