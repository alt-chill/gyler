import Test.Hspec

import qualified Gyler.CachedFileSpec
import qualified Gyler.GirarCommandSpec

import qualified Gyler.FetchSpec.BranchesQuerySpec
import qualified Gyler.FetchSpec.MaintainersQuerySpec

import qualified Gyler.Data.NonEmptyTextSpec

import qualified Gyler.Data.ValidContainer.HashSetSpec

import qualified Gyler.Domain.MaintainerSpec
import qualified Gyler.Domain.BranchSpec

main :: IO ()
main = hspec $ do
  Gyler.CachedFileSpec.spec
  Gyler.GirarCommandSpec.spec

  Gyler.FetchSpec.BranchesQuerySpec.spec
  Gyler.FetchSpec.MaintainersQuerySpec.spec

  Gyler.Data.NonEmptyTextSpec.spec

  Gyler.Data.ValidContainer.HashSetSpec.spec

  Gyler.Domain.MaintainerSpec.spec
  Gyler.Domain.BranchSpec.spec
