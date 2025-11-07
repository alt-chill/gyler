{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Gyler.Domain.Subtask (
    module Gyler.Domain.Subtask.Type,
    Subtask (..),

    pattern GearSubtask,
    pattern SRPMSubtask,
    pattern DelSubtask,

    GetSubtaskType(..),
    SomeSubtask(..),
    someSubtaskType,

    gearParser,
    srpmParser,
    delParser,

    subtaskParser
) where

-- | Module: Gyler.Domain.Subtask
--
-- Description: Type representing a Girar subtask in a brief form.
--
-- Appears in the output of `gyle task ls`:
-- > tlpui.git=1.8.0-alt1 srpm=cabal-vendor-1.1.1-alt1.src.rpm del=pandoc
--
-- It is casually a newtype wrapper around the RPM type.
--
-- A Subtask ('Gear / 'SRPM) is based on RPM 'WithEVR, while
-- a Subtask 'Del is based on RPM 'NoEVR.
--
-- Implemented using DataKinds and TypeFamilies.
-- The module provides an existential type and parsers.
--
-- See also: Gyler.Domain.RPM, Gyler.Domain.Subtask.Type

-- Base / Prelude
import GHC.Generics  (Generic)
import qualified Data.Text as T (pack, unpack, stripSuffix)
import Data.Typeable (Typeable, cast)

-- Internal data types
import Gyler.Data.NonEmptyText.QQ (net)
import qualified Gyler.Data.NonEmptyText as NET (breakOnPenultimate)

-- Domain types
import Gyler.Domain.RPM.Symbols (rpmChars)
import Gyler.Domain.RPM.Name    (nameP, mkName)
import Gyler.Domain.RPM.EVR     (evrP, mkEvr)
import Gyler.Domain.RPM         (RPM(..), EVROf, pattern N, pattern NEVR)

import Gyler.Domain.Subtask.Type

-- Parsers
import Gyler.Parsers        ( Parser, latinAlphaNum, hasPrefix
                            , require, requireRight)

import Text.Megaparsec      ((<|>), oneOf, some, try)
import Text.Megaparsec.Char (char)

-- Serialization
import Gyler.Serialize (deriveIDSerializable, deriveIDSerializableWith)
import Data.Serialize  (Serialize(..), Get)

-- Side classes
import Gyler.Classes.Renderable (Renderable(..))

-------------------------------------------------------------------------------
-- | Represents a subtask of a specific type ('Gear', 'SRPM', or 'Del').
--
--   type family HasEVRFor (t :: SubtaskType) :: EVRFlag where
--      HasEVRFor 'Gear = 'WithEVR
--      HasEVRFor 'SRPM = 'WithEVR
--      HasEVRFor 'Del  = 'NoEVR
--
newtype Subtask (t :: SubtaskType) =  Subtask {
    package :: RPM (HasEVRFor t)
}

-------------------------------------------------------------------------------
-- Typeclass Instances
--

deriving instance (Show (EVROf (HasEVRFor t))) => Show (Subtask t)
deriving instance (Eq   (EVROf (HasEVRFor t))) => Eq   (Subtask t)

deriving instance Generic (Subtask t)

$(deriveIDSerializable [t| Subtask 'Gear |])
$(deriveIDSerializable [t| Subtask 'SRPM |])
$(deriveIDSerializable [t| Subtask 'Del  |])

instance Renderable (Subtask 'Gear) where
    render (Subtask (RPM n evr)) = render n <> ".git=" <> render evr

instance Renderable (Subtask 'SRPM) where
    render (Subtask (RPM n evr)) = "srpm=" <> render n <> "-" <> render evr <> ".src.rpm"

instance Renderable (Subtask 'Del)  where
    render (Subtask (RPM n _))   = "del=" <> render n

---------------------------------------------------
-- Pattern Synonyms
--

pattern GearSubtask :: RPM (HasEVRFor 'Gear) -> Subtask 'Gear
pattern GearSubtask pkg = Subtask pkg

pattern SRPMSubtask :: RPM (HasEVRFor 'SRPM) -> Subtask 'SRPM
pattern SRPMSubtask pkg = Subtask pkg

pattern DelSubtask  :: RPM (HasEVRFor 'Del)  -> Subtask 'Del
pattern DelSubtask  pkg = Subtask pkg

{-# COMPLETE GearSubtask, SRPMSubtask, DelSubtask #-}

-------------------------------------------------------------------------------
-- Existential uwrapper
-------------------------------------------------------------------------------

class GetSubtaskType (t :: SubtaskType) where
    subtaskType :: Subtask t -> SubtaskType

instance GetSubtaskType 'Gear where
    subtaskType _ = Gear

instance GetSubtaskType 'SRPM where
    subtaskType _ = SRPM

instance GetSubtaskType 'Del where
    subtaskType _ = Del

---------------------------------------------------
-- Existential type
--

data SomeSubtask where
    SomeSubtask :: ( Serialize  (Subtask t)
                   , Show       (Subtask t)
                   , Eq         (Subtask t)
                   , Renderable (Subtask t)
                   , Typeable   (Subtask t)
                   , GetSubtaskType t
                   )
                    => Subtask t -> SomeSubtask

deriving instance Show SomeSubtask

instance Eq SomeSubtask where
    (SomeSubtask a) == (SomeSubtask b) = Just a == cast b

$(deriveIDSerializableWith
    [|
       \(SomeSubtask x) -> do
           put (subtaskType x)
           put x
    |]


    [| do
       t <- get :: Get SubtaskType
       case t of
           Gear -> fmap SomeSubtask (get :: Get (Subtask 'Gear))
           SRPM -> fmap SomeSubtask (get :: Get (Subtask 'SRPM))
           Del  -> fmap SomeSubtask (get :: Get (Subtask 'Del))
    |]

    [t| SomeSubtask |]
 )

instance Renderable SomeSubtask where
    render (SomeSubtask x) = render x

someSubtaskType :: SomeSubtask -> SubtaskType
someSubtaskType (SomeSubtask x) = subtaskType x

-------------------------------------------------------------------------------
-- | Parses "tlpui.git=1.8.0-alt1"
-------------------------------------------------------------------------------
gearParser :: Parser (Subtask 'Gear)
gearParser = do
    -- repo == "tlpui.git"
    repo <- T.pack <$> some (latinAlphaNum <|> oneOf rpmChars)

    -- n' == "tlpui"
    n' <- require ("Invalid GEAR: repo must contain '.git' as separator. Repo: " <> T.unpack repo)
        (T.stripSuffix ".git" repo)

    _ <- char '='
    evr <- evrP

    -- Build RPM object
    rpm <- requireRight $ do
        n   <- mkName n'
        pure $ NEVR n evr

    pure $ GearSubtask rpm

-------------------------------------------------------------------------------
-- | Parses "srpm=cabal-vendor-1.1.1-alt1.src.rpm"
-------------------------------------------------------------------------------
srpmParser :: Parser (Subtask 'SRPM)
srpmParser = hasPrefix "srpm=" $ do
    -- filename == "cabal-vendor-1.1.1-alt1.src.rpm"
    filename <- T.pack <$> some (latinAlphaNum <|> oneOf rpmChars)

    -- nevr == "cabal-vendor-1.1.1-alt1
    nevr <- require ("Invalid SRPM: must end with .src.rpm\nFilename is " <> T.unpack filename)
        (T.stripSuffix ".src.rpm" filename)

    -- (n', evr') == ("cabal-vendor", "1.1.1-alt1")
    (n', evr') <- require ("Invalid SRPM: cannot split NEVR part\nNEVR is " <> T.unpack nevr)
        (NET.breakOnPenultimate [net|-|] nevr)

    -- Build RPM object
    rpm <- requireRight $ do
        n   <- mkName n'
        evr <- mkEvr  evr'
        pure $ NEVR n evr

    pure $ SRPMSubtask rpm

-------------------------------------------------------------------------------
-- | Parses "del=pandoc"
-------------------------------------------------------------------------------
delParser :: Parser (Subtask 'Del)
delParser = hasPrefix "del=" $ do
    n <- nameP
    pure . DelSubtask $ N n

-------------------------------------------------------------------------------
-- | Parses any subtask: Gear, SRPM, or Del.
-------------------------------------------------------------------------------
subtaskParser :: Parser SomeSubtask
subtaskParser =
        try (SomeSubtask <$> gearParser)
    <|> try (SomeSubtask <$> srpmParser)
    <|> try (SomeSubtask <$> delParser)
