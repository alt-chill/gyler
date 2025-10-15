module Gyler.Domain.RPM.VerCmp (
    rpmvercmp
) where

-- | Module: Gyler.Domain.RPM.VerCmp
--
-- Description: Pure implementation of rpmvercmp algorithm

import Gyler.Classes.IsText (IsText(..))
import Gyler.Domain.RPM.Symbols (separators)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)

-- Represents a version component: alphabetic, numeric, or tilde (~)
data Segment
    = AlphaSegment   !Text
    | NumericSegment !Text
    | TildeSegment
    deriving (Show, Eq)

-- Defines custom ordering between segments, following RPM logic
instance Ord Segment where
    compare TildeSegment TildeSegment = EQ
    compare _ TildeSegment            = GT
    compare TildeSegment _            = LT

    compare (AlphaSegment a)   (AlphaSegment b)   = compare a b
    compare (AlphaSegment _)   (NumericSegment _) = GT
    compare (NumericSegment _) (AlphaSegment _)   = LT
    compare (NumericSegment a) (NumericSegment b) = compareNumeric a b

-- Numeric comparison: ignore leading zeros, then compare by length and lexicographically
compareNumeric :: Text -> Text -> Ordering
compareNumeric a b =
    let a' = T.dropWhile (== '0') a
        b' = T.dropWhile (== '0') b
        la = T.length a'
        lb = T.length b'
    in case compare la lb of
         EQ -> compare a' b'
         r  -> r

compareLists :: [Segment] -> [Segment] -> Ordering
compareLists = compareLists'
    where
    compareLists' (x:xs) (y:ys) = compare x y <> compareLists xs ys

    compareLists' (TildeSegment:_)  []               = LT
    compareLists' []                (TildeSegment:_) = GT

    compareLists' (_:_) []    = GT
    compareLists' []    (_:_) = LT

    compareLists' []    []    = EQ

-- Detects if a character is a Latin alphabetic letter
isLatinAlpha :: Char -> Bool
isLatinAlpha c = isAsciiLower c || isAsciiUpper c

-- Extracts the next segment (alphabetic, numeric, or tilde) from the text
nextSegment :: Text -> Maybe (Segment, Text)
nextSegment txt = case T.uncons txt of
    Nothing -> Nothing
    Just (x, xs)
      | x == '~'       -> Just (TildeSegment, xs)
      | isLatinAlpha x -> let (seg, rest) = T.span isLatinAlpha txt in Just (AlphaSegment seg, rest)
      | isDigit x      -> let (seg, rest) = T.span isDigit txt      in Just (NumericSegment seg, rest)
      | otherwise      -> nextSegment xs


-- Single-pass splitter: breaks version string into ordered segments
splitter :: IsText e => e -> [Segment]
splitter = go . toText
  where
    go t
      | T.null t = []
      | otherwise =
          case nextSegment t of
            Just (seg, rest) -> seg : go rest
            Nothing -> []

-- Main RPM-style version comparator
rpmvercmp :: (IsText e1, IsText e2) => e1 -> e2 -> Ordering
rpmvercmp str1 str2 =
    compareLists (splitter str1) (splitter str2)
