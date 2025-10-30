{-# LANGUAGE TemplateHaskellQuotes #-}

module Gyler.Serialize.UniqID (
    HasUniqID (..),
    deriveUniqID
) where

-- Module: Gyler.Serialize.UniqID
--
-- Description: Unique ID class for Gyler domain entities.
--
-- Instances of this class can only be created using the `deriveUniqID`
-- Template Haskell function.
--
-- ID "uniqueness" is ensured by computing a hash of the type name.
-- Technically, collisions are possible, but the number of domain entities
-- is significantly smaller than the range of Word16 values.
-- Therefore, these IDs are sufficiently unique for serialization purposes.

import Language.Haskell.TH (TypeQ, Q, Dec, Exp, pprint)

import Data.Word (Word16)
import Data.Hashable (hashWithSalt)
import Data.Proxy (Proxy)

-- This class is *not exported*. It prevents users from creating
-- their own HasUniqID instances outside this module.
class SealedUniqID t

------------------------------------------------------------
-- Public class
------------------------------------------------------------

class SealedUniqID t => HasUniqID t where
    uniqID :: Proxy t -> Word16

------------------------------------------------------------
-- Deterministic uniqID generator
------------------------------------------------------------

genDeterministicID :: TypeQ -> Q Exp
genDeterministicID tQ = do
    ty <- tQ
    let str = pprint ty
        h   = (fromIntegral . hashWithSalt 42) str :: Word16
    [| h |]

------------------------------------------------------------
-- Template Haskell derivation
------------------------------------------------------------

deriveUniqID :: TypeQ -> Q [Dec]
deriveUniqID tQ = do
    let uid = genDeterministicID tQ
    [d|
        instance SealedUniqID $tQ

        instance HasUniqID $tQ where
            uniqID _ = $uid
      |]
