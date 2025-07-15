import Test.Hspec

import qualified Gyler.CachedFileSpec
import qualified Gyler.GirarCommandSpec

import qualified Gyler.GirarEntity.BranchesSpec
import qualified Gyler.GirarEntity.MaintainersSpec

import qualified Gyler.Data.NonEmptyTextSpec

main :: IO ()
main = hspec $ do
  Gyler.CachedFileSpec.spec
  Gyler.GirarCommandSpec.spec

  Gyler.GirarEntity.BranchesSpec.spec
  Gyler.GirarEntity.MaintainersSpec.spec

  Gyler.Data.NonEmptyTextSpec.spec
