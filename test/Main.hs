import Test.Hspec

import qualified Gyler.CachedFileSpec
import qualified Gyler.GirarCommandSpec

import qualified Gyler.Data.NonEmptyTextSpec

import qualified Gyler.Data.ValidContainer.HashSetSpec

import qualified Gyler.Domain.MaintainerSpec
import qualified Gyler.Domain.PushableBranchSpec

import qualified Gyler.LoggingSpec

import TestUtils.FetchSpec.Template

import Data.IORef
import qualified Data.Map as Map

import Gyler.FetchSpec.MaintainersQuery      (MaintainersQuery(..))
import Gyler.FetchSpec.PushableBranchesQuery (PushableBranchesQuery(..))

main :: IO ()
main = hspec $ do
  Gyler.CachedFileSpec.spec
  Gyler.GirarCommandSpec.spec

  Gyler.Data.NonEmptyTextSpec.spec

  Gyler.Data.ValidContainer.HashSetSpec.spec

  Gyler.Domain.MaintainerSpec.spec
  Gyler.Domain.PushableBranchSpec.spec

  Gyler.LoggingSpec.spec

  beforeAll (newIORef Map.empty) $ do
    mkFetchSpecTest MaintainersQuery
    mkFetchSpecTest PushableBranchesQuery
