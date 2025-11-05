{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Gyler.Serialize (
    IDSerializer(..),
    deriveIDSerializer, deriveIDSerializerWith,
    deriveIDSerializable, deriveIDSerializableWith
) where

-- Module: Gyler.Serialize
--
-- Description: ID serialization class for Gyler domain entities.
--
-- This module is based on `Data.Serialize` from the `cereal` package.
--
-- The main difference is that each instance of `IDSerializer` is required
-- to include a unique identifier at the beginning of its binary representation.
--
-- Instances of `IDSerializer` can only be created using Template Haskell macros.
-- These macros generate both `IDSerializer` and `Serialize` instances for the specified type.
--
-- This approach allows the use of all functions from `Data.Serialize`
-- while ensuring that different types cannot be confused.
--
-- The type must have a `Generic` instance in order to use `deriveIDSerializer`
-- or `deriveIDSerializable`. If a type cannot be represented as `Generic`,
-- use `deriveIDSerializerWith` instead.
--
-- Example:
--      data MyType = MyType Int String deriving (Generic)
--      $(deriveIDSerializable [t| MyType |])
--
--      $(deriveIDSerializableWith
--          [|
--             \(SomeSubtask x) -> do
--                 put (subtaskType x)
--                 put x
--          |]
--
--
--          [| do
--             t <- get :: Get SubtaskType
--             case t of
--                 Gear -> fmap SomeSubtask (get :: Get (Subtask 'Gear))
--                 SRPM -> fmap SomeSubtask (get :: Get (Subtask 'SRPM))
--                 Del  -> fmap SomeSubtask (get :: Get (Subtask 'Del))
--          |]
--
--          [t| SomeSubtask |]
--       )
--

import Gyler.Serialize.UniqID (UniqID, HasUniqID(..), deriveUniqID)

import Language.Haskell.TH (TypeQ, Q, Dec, ExpQ)
import Language.Haskell.TH.Syntax

import Data.Proxy (Proxy(..))
import Data.Serialize (Serialize(..), Get, Putter, GSerializePut(..), GSerializeGet(..))

import GHC.Generics (Generic, Rep, from, to)

------------------------------------------------------------
-- Base class definition
------------------------------------------------------------

-- This class is *not exported*. It prevents creation of IDSerializer
-- instances outside this module.
class SealedIDSerializer t

class (SealedIDSerializer t, HasUniqID t) => IDSerializer t where
    get' :: Get t
    put' :: Putter t

    default put' :: (Generic t, GSerializePut (Rep t)) => Putter t
    put' = gPut . from

    default get' :: (Generic t, GSerializeGet (Rep t)) => Get t
    get' = to <$> gGet

------------------------------------------------------------
-- Common Serialize helpers
------------------------------------------------------------

putWithID :: forall t. IDSerializer t => Putter t
putWithID x = put (uniqID (Proxy @t)) >> put' x

getWithID :: forall t. IDSerializer t => Get t
getWithID = do
    n <- get :: Get UniqID
    if n == uniqID (Proxy @t)
        then get'
        else fail $ "UniqID mismatch: expected " ++ show (uniqID (Proxy @t)) ++ ", got " ++ show n

------------------------------------------------------------
-- Template Haskell derivation
------------------------------------------------------------

deriveIDSerializer :: TypeQ -> Q [Dec]
deriveIDSerializer tQ = [d|
    instance SealedIDSerializer $tQ

    instance IDSerializer $tQ

    instance Serialize $tQ where
        put = putWithID
        get = getWithID
  |]

deriveIDSerializerWith :: ExpQ -> ExpQ -> TypeQ -> Q [Dec]
deriveIDSerializerWith putExp getExp tQ = [d|
    instance SealedIDSerializer $tQ
    instance IDSerializer $tQ where
        put' = $putExp
        get' = $getExp
    instance Serialize $tQ where
        put = putWithID
        get = getWithID
  |]

------------------------------------------------------------
-- Combined derivation (UniqID + Serialize)
------------------------------------------------------------

deriveIDSerializable :: TypeQ -> Q [Dec]
deriveIDSerializable tQ = do
    uniq <- deriveUniqID tQ
    ser  <- deriveIDSerializer tQ
    pure (uniq ++ ser)

deriveIDSerializableWith :: ExpQ -> ExpQ -> TypeQ -> Q [Dec]
deriveIDSerializableWith putExp getExp tQ = do
    uniq <- deriveUniqID tQ
    ser  <- deriveIDSerializerWith putExp getExp tQ
    pure (uniq ++ ser)
