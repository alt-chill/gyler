{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Gyler.SerializeSpec (spec) where

-- | Module: Gyler.SerializeSpec
--
-- Description: Auto-generated Serialize round-trip tests for all
-- types that have an "IDSerializable" instance.
--
-- Based on the Gyler.Serialize.UniqID.All module.
--
-- After the Template Haskell expansion, this spec will generate
-- something like:
--
-- > spec = describe "Serialize instances" $ parallel $ do
-- >     mkSerializeTest (Proxy :: Proxy Epoch)
-- >     mkSerializeTest (Proxy :: Proxy Version)
-- >     mkSerializeTest (Proxy :: Proxy Release)
-- >     ...
--
-- See also `mkSerializeTest` in `TestUtils.Serialize.Template`.

import Test.Hspec
import TestUtils.Serialize.Template (mkSerializeTest)

import Gyler.Arbitraries ()
import Gyler.Serialize.UniqID.All (allUniqIDTypes)

import Gyler.Serialize.UniqID (UniqID)

import Language.Haskell.TH

import Data.Proxy (Proxy(..))

import Data.Proxy (Proxy(..))
import Data.IORef (newIORef)
import qualified Data.Map as Map

spec :: Spec
spec = beforeAll (newIORef Map.empty) $
    describe "Serialize instances"  $ do
        $(do
            doE [ noBindS [| mkSerializeTest (Proxy :: Proxy $t) |]
                | t <- allUniqIDTypes
                ])
