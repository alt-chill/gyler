module Gyler.Domain.PushableBranchSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Gyler.Domain.PushableBranch
import Gyler.Classes.RuntimeValidated.Internal
import Gyler.Data.ValidContainer.HashSet (HashSet)
import qualified Gyler.Data.ValidContainer as VC

import Gyler.Data.NonEmptyText (NonEmptyText)
import Gyler.Data.NonEmptyText.Arbitrary ()
import Data.Maybe (isNothing)

mkSet :: [NonEmptyText] -> HashSet PushableBranch
mkSet = mkValidSet

mkM :: NonEmptyText -> PushableBranch
mkM = mkUnsafe

spec :: Spec
spec = describe "PushableBranch as RuntimeValidated" $ do
  it "mkValidated succeeds for values in mkValidSet" $
    property $ \xs ->
      not (null xs) ==>
        forAll (elements xs) $ \x ->
          mkValidated (mkSet xs) x == Just (mkM x)

  it "mkValidated fails for values not in mkValidSet" $
    property $ \xs y ->
      let set = mkSet xs
       in (y `elem` xs) || isNothing (mkValidated set y)
