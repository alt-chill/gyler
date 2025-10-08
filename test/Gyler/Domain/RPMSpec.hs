module Gyler.Domain.RPMSpec (
    spec
) where

import qualified Gyler.Domain.RPM.EVRSpec as EVR

import Test.Hspec

spec :: Spec
spec = do
    EVR.spec
