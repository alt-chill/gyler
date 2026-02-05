{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module TestUtils.RuntimeValidated.Template (mkRuntimeValidatedTest) where

import Test.Hspec
import Test.QuickCheck

import Gyler.Classes.RuntimeValidated (RuntimeValidated(..), mkValidated, mkValidSet)
import Gyler.Classes.RuntimeValidated.Internal (RuntimeValidated(mkUnsafe))
import Gyler.Classes.RuntimeValidated.Environment (HasValidSetFor(..))

import Gyler.Classes.RuntimeValidated.Parser (runtimeValidatedParser)
import Gyler.Classes.RuntimeValidated.Parser.Raw (HasParsableRaw)

import Gyler.Classes.IsText (IsText(..))

import Gyler.Parsers (Parser, useParser)

import Gyler.Data.ValidContainer.HashSet (HashSet, fromList)

import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(..))

import Data.Maybe (isNothing)
import Data.Either (isRight, isLeft)

import qualified Data.Text as T (Text, pack)
import qualified Gyler.Data.NonEmptyText as NET (NonEmptyText, pack)

-- Generator of valid text instances
validChar :: Gen Char
validChar = elements $
    ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

genValidText :: Gen NET.NonEmptyText
genValidText = (\(Just x) -> x)  . NET.pack <$> listOf1 validChar

genValidTextSet :: Gen [NET.NonEmptyText]
genValidTextSet = listOf1 genValidText

-- | Local wrapper to act as an Environment for HasValidSetFor
newtype TestEnv a = TestEnv (HashSet a)

instance (RuntimeValidated a) => HasValidSetFor a (TestEnv a) where
    type ValidSet a (TestEnv a) = HashSet a
    getValidSet (TestEnv s) = s

mkRuntimeValidatedTest
  :: forall a .
     ( RuntimeValidated a
     , HasParsableRaw a
     , Eq a
     , Show a
     , Arbitrary (Raw a)
     , Eq (Raw a)
     , Show (Raw a)
     , Typeable a
     , Raw a ~ NET.NonEmptyText
     )
  => Proxy a -> Spec
mkRuntimeValidatedTest _ =
  describe (show (typeRep (Proxy :: Proxy a)) ++ " as RuntimeValidated") $ do
    describe "mkValidated" $ do
        it "succeeds for values in mkValidSet" $
          property $ \(xs :: [Raw a]) ->
            not (null xs) ==>
              forAll (elements xs) $ \x ->
                mkValidated (mkValidSet xs :: HashSet a) x
                  == Just (mkUnsafe x)

        it "fails for values not in mkValidSet" $
          property $ \(xs :: [Raw a]) (y :: Raw a) ->
            let set = mkValidSet xs :: HashSet a
            in (y `elem` xs) || isNothing (mkValidated set y)

    describe "runtimeValidatedParser" $ do
        it "succeeds for valid values" $
            property $ forAll genValidTextSet $ \xs ->
                let validSet :: HashSet a
                    validSet = mkValidSet xs :: HashSet a

                    env :: TestEnv a
                    env = TestEnv validSet

                    parser :: Parser a
                    parser = runtimeValidatedParser env

                    res :: [a]
                    Right res = mapM (useParser parser . toText) xs
                in fromList res == validSet

        it "fails on invalid values" $
            property $ forAll genValidTextSet $ \xs ->
                let validSet :: HashSet a
                    validSet = mkValidSet xs :: HashSet a

                    env :: TestEnv a
                    env = TestEnv validSet

                    parser :: Parser a
                    parser = runtimeValidatedParser env

                in forAll genValidText $ \x ->
                    (x `elem` xs)    && isRight (useParser parser (toText x)) ||
                    (x `notElem` xs) && isLeft  (useParser parser (toText x))
