module Gyler.Serialize.Text (

) where

-- |
-- Module: Gyler.Serialize.Text
--
-- Description: Orphan Serialize instance for Text

import Data.Text (Text)
import Data.Serialize (Serialize(..))

import qualified Data.Text.Encoding as TE (encodeUtf8, decodeUtf8)

instance Serialize Text where
      put = put . TE.encodeUtf8
      get = TE.decodeUtf8 <$> get
