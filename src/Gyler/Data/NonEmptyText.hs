-- Copyright (c) 2022 Trevis Elser

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Gyler.Data.NonEmptyText
    ( NonEmptyText

      -- * Creation
    , new
    , singleton
    , toText
    , fromText

      -- * Basic interface
    , cons
    , snoc
    , uncons
    , unsnoc
    , append
    , Gyler.Data.NonEmptyText.head
    , Gyler.Data.NonEmptyText.last
    , Gyler.Data.NonEmptyText.tail
    , Gyler.Data.NonEmptyText.init
    , Gyler.Data.NonEmptyText.length
    , Gyler.Data.NonEmptyText.map
    , isSingleton

    -- * Folds
    , Gyler.Data.NonEmptyText.foldl1
    , Gyler.Data.NonEmptyText.foldl1'

    -- * Breaking into lines and words
    , Gyler.Data.NonEmptyText.lines
    , Gyler.Data.NonEmptyText.words
    , Gyler.Data.NonEmptyText.unlines
    , Gyler.Data.NonEmptyText.unwords

    -- * Converting to String
    , Gyler.Data.NonEmptyText.pack
    , Gyler.Data.NonEmptyText.unpack
    ) where

import Control.DeepSeq (NFData)
import Data.Bifunctor ( bimap )

import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE

import GHC.Generics (Generic)

import Data.Maybe (mapMaybe)

import Language.Haskell.TH.Syntax (Lift)
import Data.Hashable (Hashable)

import Data.Serialize (Serialize (..))

instance Serialize Text.Text where
    put = put . TE.encodeUtf8
    get = TE.decodeUtf8 <$> get

data NonEmptyText =
  NonEmptyText !Char !Text.Text
  deriving (Eq, Ord, NFData, Generic, Lift, Hashable)

instance Serialize NonEmptyText

instance Show NonEmptyText where
  show = show . toText

instance Semigroup NonEmptyText where
  x <> y = append x y

-- | /O(1)/ Create a new 'NonEmptyText'
--
-- >>> new 'h' "ello world"
-- "hello world"
--
new :: Char -> Text.Text -> NonEmptyText
new = NonEmptyText


-- | /O(1)/ Convert a character into a 'NonEmptyText'.
--
-- >>> singleton 'a'
-- "a"
--
singleton :: Char -> NonEmptyText
singleton = flip NonEmptyText Text.empty


-- | /O(1)/ Check if the string is composed of only one character
isSingleton :: NonEmptyText -> Bool
isSingleton = Text.null . snd . uncons


-- | /O(n)/ Prefixes the 'NonEmptyText' with one character
cons :: Char -> NonEmptyText -> NonEmptyText
cons h t = new h (toText t)


-- | /O(n)/ Suffixes the 'NonEmptyText' with one character
snoc :: NonEmptyText -> Char -> NonEmptyText
snoc (NonEmptyText h t) c = new h (Text.snoc t c)


-- | /O(n)/ Appends one 'NonEmptyText' to another
--
-- >>> append <$> fromText "hello," <*> fromText " world."
-- Just "hello, world."
append :: NonEmptyText -> NonEmptyText -> NonEmptyText
append (NonEmptyText h t) = new h . Text.append t . toText


-- | /O(1)/ Return the first character and the rest of the 'NonEmptyText'
uncons :: NonEmptyText -> (Char, Text.Text)
uncons (NonEmptyText h t) = (h, t)


-- | /O(n)/ Return the beginning of the 'NonEmptyText', and its last character
unsnoc :: NonEmptyText -> (Text.Text, Char)
unsnoc (NonEmptyText h t) =
    case unsnocT t of
        Nothing     -> (Text.empty, h)
        Just (m, e) -> (Text.cons h m, e)
  where
    unsnocT :: Text.Text -> Maybe (Text.Text, Char)
    unsnocT text = -- Some old version of Data.Text don't have unsnoc
        let n = Text.length text - 1 in
        if Text.null text
        then Nothing
        else Just (Text.take n text, Text.index text n)


-- | /O(1)/ Return the first of the 'NonEmptyText'
--
-- As opposed to 'Data.Text.head', this is guaranteed to succeed, as the
-- the text is never empty.
head :: NonEmptyText -> Char
head (NonEmptyText h _) = h
{-# INLINE Gyler.Data.NonEmptyText.head #-}

-- | /O(1)/ Return the last character of the 'NonEmptyText'
--
-- This never fails.
last :: NonEmptyText -> Char
last = snd . unsnoc
{-# INLINE Gyler.Data.NonEmptyText.last #-}


-- | /O(1)/ Return all characters of the 'NonEmptyText' but the first one
tail :: NonEmptyText -> Text.Text
tail (NonEmptyText _ t ) = t
{-# INLINE Gyler.Data.NonEmptyText.tail #-}


-- | /O(n)/ Return all character of the 'NonEmptyText' but the last one
init :: NonEmptyText -> Text.Text
init = fst . unsnoc
{-# INLINE Gyler.Data.NonEmptyText.init #-}

-- | /O(n)/ Return the length of the total 'NonEmptyText'.
length :: NonEmptyText -> Int
length = (1 +) . Text.length . Gyler.Data.NonEmptyText.tail
{-# INLINE Gyler.Data.NonEmptyText.length #-}

-- | /O(n)/ Convert to NonEmptyText to Text.
--
-- The 'Data.Text.Text' result is guaranteed to be non-empty. However, this is
-- not reflected in the type.
toText :: NonEmptyText -> Text.Text
toText = uncurry Text.cons . uncons


-- | /O(n)/ 'Gyler.Data.NonEmptyText.map' @f@ @t@ is the 'NonEmptyText' obtained by applying @f@ to
-- each element of @t@.
map :: (Char -> Char) -> NonEmptyText -> NonEmptyText
map f = uncurry new . bimap f (Text.map f) . uncons
{-# INLINE Gyler.Data.NonEmptyText.map #-}

-- | /O(n)/ Create a 'NonEmptyText' from 'Data.Text.Text'.
--
-- If the original text is empty, this will return 'Data.Maybe.Nothing'.
--
-- >>> fromText "hello"
-- Just "hello"
-- >>> fromText ""
-- Nothing
fromText :: Text.Text -> Maybe NonEmptyText
fromText = fmap (uncurry NonEmptyText) . Text.uncons

-- | /O(n)/ 'Gyler.Data.NonEmptyText.foldl1' is a left associative fold with no base case, as we know the
-- text cannot be empty.
foldl1 :: (Char -> Char -> Char) -> NonEmptyText -> Char
foldl1 fn (NonEmptyText h t) = Text.foldl fn h t
{-# INLINE Gyler.Data.NonEmptyText.foldl1 #-}

-- | /O(n)/ A strict version of 'Gyler.Data.NonEmptyText.foldl1'.
foldl1' :: (Char -> Char -> Char) -> NonEmptyText -> Char
foldl1' fn (NonEmptyText h t) = Text.foldl' fn h t
{-# INLINE Gyler.Data.NonEmptyText.foldl1' #-}


-- | /O(n)/ Break a 'NonEmptyText' into lines.
-- Empty lines are not discarded.
lines :: NonEmptyText -> [NonEmptyText]
lines = mapMaybe fromText . Text.lines . toText
{-# INLINE Gyler.Data.NonEmptyText.lines #-}

-- | /O(n)/ Break a 'NonEmptyText' into words, separated by whitespace.
words :: NonEmptyText -> [NonEmptyText]
words = mapMaybe fromText . Text.words . toText
{-# INLINE Gyler.Data.NonEmptyText.words #-}

-- | /O(n)/ The inverse of 'lines'.  Joins a list of 'NonEmptyText' with
--   newline characters.  Because the result may be empty when the input list
--   is empty, the function returns @Maybe NonEmptyText@.
unlines :: [NonEmptyText] -> Maybe NonEmptyText
unlines = fromText . Text.unlines . fmap toText
{-# INLINE Gyler.Data.NonEmptyText.unlines #-}

-- | /O(n)/ The inverse of 'words'.  Joins a list of 'NonEmptyText' with
--   single space characters.  Returns 'Nothing' when the input list is empty.
--
unwords :: [NonEmptyText] -> Maybe NonEmptyText
unwords = fromText . Text.unwords . fmap toText
{-# INLINE Gyler.Data.NonEmptyText.unwords #-}

-- | /O(n)/ Convert a String into a NonEmptyText.
-- Performs replacement on invalid scalar values, so unpack . pack is not id:
--
-- Because the result may be empty when the input string
-- is empty, the function returns @Maybe NonEmptyText@.
pack :: String -> Maybe NonEmptyText
pack = fromText . Text.pack
{-# INLINE Gyler.Data.NonEmptyText.pack #-}

-- /O(n)/ Convert a NonEmptyText into a String.
unpack :: NonEmptyText -> String
unpack = Text.unpack . toText
{-# INLINE Gyler.Data.NonEmptyText.unpack #-}
