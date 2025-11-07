{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Gyler.Domain.RPMSpec (
    spec
) where

import qualified Gyler.Domain.RPM.EVRSpec    as EVR
import qualified Gyler.Domain.RPM.NameSpec   as Name
import qualified Gyler.Domain.RPM.VerCmpSpec as VerCmp

import Gyler.Domain.RPM

import Test.Hspec
import Test.QuickCheck

import Data.Proxy (Proxy(..))

import Gyler.Arbitraries ()

spec :: Spec
spec = describe "RPM" $ do
    EVR.spec
    Name.spec
    VerCmp.spec
