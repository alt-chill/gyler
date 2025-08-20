module Gyler.Data.ValidContainer.HashSetSpec (spec) where

import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Test.Hspec
import qualified Gyler.Data.ValidContainer.HashSet as HS
import Data.Serialize (encode, decode)
import Gyler.Data.NonEmptyText
import Data.Hashable (Hashable)

import Gyler.Data.NonEmptyText.Arbitrary ()

arbitraryHashSet :: Gen (HS.HashSet NonEmptyText)
arbitraryHashSet = HS.fromList <$> arbitrary

spec :: Spec
spec = do
  describe "Serialize HashSet NonEmptyText" $ do

    it "roundtrip: encode . decode == id" $ property $
      \hs -> let hsSet = HS.fromList (hs :: [NonEmptyText])
                 encoded = encode hsSet
                 decoded = decode encoded :: Either String (HS.HashSet NonEmptyText)
             in decoded == Right hsSet

    it "empty HashSet serializes correctly" $ do
      let hsEmpty = HS.empty :: HS.HashSet NonEmptyText
          encoded = encode hsEmpty
          decoded = decode encoded :: Either String (HS.HashSet NonEmptyText)
      decoded `shouldBe` Right hsEmpty

    it "size is preserved after encode/decode" $ property $
      \hs -> let hsSet = HS.fromList (hs :: [NonEmptyText])
                 encoded = encode hsSet
                 decoded = decode encoded :: Either String (HS.HashSet NonEmptyText)
             in case decoded of
                  Left _ -> False
                  Right hs' -> HS.size hs' == HS.size hsSet
