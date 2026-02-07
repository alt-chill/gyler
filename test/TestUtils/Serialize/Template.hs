{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module TestUtils.Serialize.Template (mkSerializeTest) where

import Test.Hspec
import Test.QuickCheck

import Data.Serialize (Serialize, encode, decode)
import Gyler.Serialize.UniqID (UniqID, HasUniqID(..))

import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(..))

import Data.IORef
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)

mkSerializeTest
  :: forall a.
     ( Serialize a
     , HasUniqID a
     , Eq a
     , Show a
     , Arbitrary a
     , Typeable a
     )
  => Proxy a -> SpecWith (IORef (Map.Map UniqID String))
mkSerializeTest p = describe typeName $ do
    it "Serialize round-trip" $ \_ -> do
        property $ \(x :: a) ->
            decode (encode x) == Right x

    it "UniqID is unique" $ \seenRef -> do
        let currentId = uniqID p

        seen <- liftIO $ readIORef seenRef

        case Map.lookup currentId seen of
            Just existingType ->
                    expectationFailure $
                         "UniqID collision: ID " ++ show currentId ++
                         " is used by both " ++ existingType ++ " and " ++ typeName
            Nothing ->
                liftIO $ atomicModifyIORef' seenRef $ \m -> (Map.insert currentId typeName m, ())

  where
    typeName = show (typeRep (Proxy :: Proxy a))
