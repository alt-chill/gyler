import Test.Hspec

import qualified Gyler.CachedFileSpec
import qualified Gyler.GirarCommandSpec


import qualified Gyler.Data.NonEmptyTextSpec

import qualified Gyler.Data.ValidContainer.HashSetSpec

import qualified Gyler.Domain.MaintainerSpec

main :: IO ()
main = hspec $ do
  Gyler.CachedFileSpec.spec
  Gyler.GirarCommandSpec.spec


  Gyler.Data.NonEmptyTextSpec.spec

  Gyler.Data.ValidContainer.HashSetSpec.spec

  Gyler.Domain.MaintainerSpec.spec
