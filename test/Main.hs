import Test.Hspec

import qualified Gyler.CachedFileSpec
import qualified Gyler.GirarCommandSpec

import qualified Gyler.FetchSpec.PushableBranchesQuerySpec
import qualified Gyler.FetchSpec.MaintainersQuerySpec

import qualified Gyler.Data.NonEmptyTextSpec

import qualified Gyler.Data.ValidContainer.HashSetSpec

import qualified Gyler.Domain.MaintainerSpec
import qualified Gyler.Domain.PushableBranchSpec

import qualified Gyler.LoggingSpec

main :: IO ()
main = hspec $ do
  Gyler.CachedFileSpec.spec
  Gyler.GirarCommandSpec.spec

  Gyler.FetchSpec.PushableBranchesQuerySpec.spec
  Gyler.FetchSpec.MaintainersQuerySpec.spec

  Gyler.Data.NonEmptyTextSpec.spec

  Gyler.Data.ValidContainer.HashSetSpec.spec

  Gyler.Domain.MaintainerSpec.spec
  Gyler.Domain.PushableBranchSpec.spec

  Gyler.LoggingSpec.spec
