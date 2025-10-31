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

import TestUtils.Serialize.Template (mkSerializeTest)
import Data.Proxy (Proxy(..))

instance Arbitrary (RPM 'WithEVR) where
    arbitrary = NEVR <$> arbitrary <*> arbitrary

instance Arbitrary (RPM 'NoEVR) where
    arbitrary = N <$> arbitrary

spec :: Spec
spec = describe "RPM" $ do
    EVR.spec
    Name.spec
    VerCmp.spec

    mkSerializeTest (Proxy :: Proxy (RPM 'WithEVR))
    mkSerializeTest (Proxy :: Proxy (RPM 'NoEVR))
