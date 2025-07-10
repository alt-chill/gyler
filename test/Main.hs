import Test.Hspec

import qualified Gyler.CachedFileSpec
import qualified Gyler.GirarCommandSpec

import qualified Gyler.GirarEntity.BranchesSpec
import qualified Gyler.GirarEntity.MaintainersSpec

main :: IO ()
main = hspec $ do
  Gyler.CachedFileSpec.spec
  Gyler.GirarCommandSpec.spec

  Gyler.GirarEntity.BranchesSpec.spec
  Gyler.GirarEntity.MaintainersSpec.spec
