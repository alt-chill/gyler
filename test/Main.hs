import Test.Hspec

import qualified Gyler.CachedFileSpec
import qualified Gyler.GirarCommandSpec

import qualified Gyler.Data.NonEmptyTextSpec

import qualified Gyler.Data.ValidContainer.HashSetSpec

import qualified Gyler.LoggingSpec
import qualified Gyler.SerializeSpec

import qualified Gyler.Domain.RPMSpec
import qualified Gyler.Domain.SubtaskSpec

import Gyler.Data.NonEmptyText.Arbitrary ()

import TestUtils.RuntimeValidated.Template (mkRuntimeValidatedTest)

import Gyler.Domain.Maintainer (Maintainer)
import Gyler.Domain.PushableBranch (PushableBranch)
import Gyler.Domain.Branch (Branch)
import Gyler.Domain.State (State)

import TestUtils.FetchSpec.Template

import Gyler.FetchSpec.MaintainersQuery      (MaintainersQuery(..))
import Gyler.FetchSpec.BranchesQuery         (BranchesQuery(..))
import Gyler.FetchSpec.PushableBranchesQuery (PushableBranchesQuery(..))
import Gyler.FetchSpec.StatesQuery           (StatesQuery(..))

import Data.IORef
import qualified Data.Map as Map

import Data.Proxy (Proxy(..))

main :: IO ()
main = hspec $ do
  Gyler.CachedFileSpec.spec
  Gyler.GirarCommandSpec.spec

  Gyler.Data.NonEmptyTextSpec.spec

  Gyler.Data.ValidContainer.HashSetSpec.spec

  mkRuntimeValidatedTest (Proxy :: Proxy Maintainer)
  mkRuntimeValidatedTest (Proxy :: Proxy Branch)
  mkRuntimeValidatedTest (Proxy :: Proxy PushableBranch)
  mkRuntimeValidatedTest (Proxy :: Proxy State)

  Gyler.LoggingSpec.spec
  Gyler.SerializeSpec.spec

  beforeAll (newIORef Map.empty) $ do
    mkFetchSpecTest MaintainersQuery
    mkFetchSpecTest BranchesQuery
    mkFetchSpecTest PushableBranchesQuery
    mkFetchSpecTest StatesQuery

  Gyler.Domain.RPMSpec.spec
  Gyler.Domain.SubtaskSpec.spec
