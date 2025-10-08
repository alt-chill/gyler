module Gyler.Domain.RPM.EVRSpec (
    spec
) where

import qualified Gyler.Domain.RPM.EVR.EpochSpec   as Epoch
import qualified Gyler.Domain.RPM.EVR.VersionSpec as Version
import qualified Gyler.Domain.RPM.EVR.ReleaseSpec as Release

import Test.Hspec

spec :: Spec
spec = do
    Epoch.spec
    Version.spec
    Release.spec
