import Test.Hspec

import qualified Gyler.CachedFileSpec

main :: IO ()
main = hspec $ do
  Gyler.CachedFileSpec.spec
