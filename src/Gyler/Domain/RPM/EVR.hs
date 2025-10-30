{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Gyler.Domain.RPM.EVR (
    EVR(..),
    evrP,
    mkEvr
) where

import Gyler.Domain.RPM.EVR.Epoch   (Epoch(..), epochP)
import Gyler.Domain.RPM.EVR.Version (Version, versionP)
import Gyler.Domain.RPM.EVR.Release (Release, releaseP)

import Gyler.Domain.RPM.VerCmp (rpmvercmp)

import Gyler.Classes.IsText     (IsText (..))
import Gyler.Classes.Renderable (Renderable(..))
import Gyler.Serialize          (deriveIDSerializable)

import Gyler.Parsers (Parser, useParser)

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)

import Data.Maybe (fromMaybe)

import Text.Megaparsec (optional, try, eof)
import Text.Megaparsec.Char (char)

import Data.Text (Text)

-- Module: Gyler.Domain.RPM.EVR
--
-- Description: EVR is used to refer to software versions in RPM,
-- consisting of up to three components:
-- * VERSION reflects the actual packaged software version.
-- * RELEASE reflects packaging revisions within that software version.
-- * EPOCH is an artificial override to allow working around versioning
--   anomalies.

data EVR = EVR {
    _epoch   :: !Epoch,
    _version :: !Version,
    _release :: !Release
} deriving (Show, Eq, Generic)

instance Hashable  EVR

instance Ord EVR where
    compare (EVR e1 v1 r1) (EVR e2 v2 r2)  = compare e1 e2
                                          <> compare v1 v2
                                          <> compare r1 r2

instance Renderable EVR where
    render (EVR e v r) = renderEpoch e <> render v <> "-" <> render r
        where
        renderEpoch :: Epoch -> Text
        renderEpoch (Epoch 0) = ""
        renderEpoch e = render e <> ":"

$(deriveIDSerializable [t| EVR |])

{-# ANN module ("HLint: ignore Use <$>" :: String) #-}
evrP :: Parser EVR
evrP = do
    e <- fromMaybe (Epoch 0) <$> (optional . try) epochP
    v <- versionP
    _ <- char '-'
    r <- releaseP

    return (EVR e v r)

mkEvr :: IsText e => e -> Either Text EVR
mkEvr txt = useParser (evrP <* eof) (toText txt)
