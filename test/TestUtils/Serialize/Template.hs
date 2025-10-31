{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module TestUtils.Serialize.Template (mkSerializeTest) where

import Test.Hspec
import Test.QuickCheck

import Data.Serialize (Serialize, encode, decode)

import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(..))

mkSerializeTest
  :: forall a proxy.
     ( Serialize a
     , Eq a
     , Show a
     , Arbitrary a
     , Typeable a
     )
  => proxy a -> Spec
mkSerializeTest _ =
  it (show (typeRep (Proxy :: Proxy a)) ++ " Serialize round-trip") $ do
      property $ \(x :: a) ->
        decode (encode x) == Right x
