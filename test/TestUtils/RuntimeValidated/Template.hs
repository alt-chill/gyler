{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module TestUtils.RuntimeValidated.Template (mkRuntimeValidatedTest) where

import Test.Hspec
import Test.QuickCheck

import Gyler.Classes.RuntimeValidated.Internal

import Gyler.Data.ValidContainer.HashSet (HashSet)

import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(..))

import Data.Maybe (isNothing)

mkRuntimeValidatedTest
  :: forall a proxy.
     ( RuntimeValidated a
     , Eq a
     , Show a
     , Arbitrary (Raw a)
     , Eq (Raw a)
     , Show (Raw a)
     , Typeable a
     )
  => proxy a -> Spec
mkRuntimeValidatedTest _ =
  describe (show (typeRep (Proxy :: Proxy a)) ++ " as RuntimeValidated") $ do
    it "mkValidated succeeds for values in mkValidSet" $
      property $ \(xs :: [Raw a]) ->
        not (null xs) ==>
          forAll (elements xs) $ \x ->
            mkValidated (mkValidSet xs :: HashSet a) x
              == Just (mkUnsafe x)

    it "mkValidated fails for values not in mkValidSet" $
      property $ \(xs :: [Raw a]) (y :: Raw a) ->
        let set = mkValidSet xs :: HashSet a
         in (y `elem` xs) || isNothing (mkValidated set y)
