module Gyler.Domain.MaintainerSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Gyler.Domain.Maintainer
import Gyler.Classes.RuntimeValidated.Internal
import Gyler.Data.ValidContainer.HashSet (HashSet)
import qualified Gyler.Data.ValidContainer as VC

import Gyler.Data.NonEmptyText (NonEmptyText)
import Gyler.Data.NonEmptyText.Arbitrary ()
import Data.Maybe (isNothing)

mkSet :: [NonEmptyText] -> HashSet Maintainer
mkSet = mkValidSet

mkM :: NonEmptyText -> Maintainer
mkM = mkUnsafe

spec :: Spec
spec = describe "Maintainer as RuntimeValidated" $ do
  it "mkValidated succeeds for values in mkValidSet" $
    property $ \xs ->
      not (null xs) ==>
        forAll (elements xs) $ \x ->
          mkValidated (mkSet xs) x == Just (mkM x)

  it "mkValidated fails for values not in mkValidSet" $
    property $ \xs y ->
      let set = mkSet xs
       in (y `elem` xs) || isNothing (mkValidated set y)
