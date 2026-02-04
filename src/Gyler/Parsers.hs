-- |
-- Module: Gyler.Parsers
-- Description: basic parsers combinators, built on top of `Megaparsec`

module Gyler.Parsers (
    module Gyler.Parsers.Type,

    useParser,
    lexeme,
    symbol,
    word,

    nonEmptyText,

    latinAlpha,
    latinAlphaNum,

    hasPrefix,
    require,
    requireRight
) where

import Data.Text (Text)

import qualified Data.Text as T (pack, unpack)

import Text.Megaparsec (satisfy, some, (<|>), empty, parse, eof, errorBundlePretty)
import Text.Megaparsec.Char (space1, alphaNumChar, markChar, digitChar, string)
import qualified Text.Megaparsec.Char.Lexer as L (lexeme, symbol, space)

import Gyler.Data.NonEmptyText (NonEmptyText, fromText)
import Data.Char (isAsciiLower, isAsciiUpper)

import Gyler.Parsers.Type (Parser(..))

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

word :: Parser Text
word = lexeme $ T.pack <$> some alphaNumChar

useParser :: Parser a -> Text -> Either Text a
useParser p s =
    case parse p "" s of
        Left  err -> Left . T.pack $ errorBundlePretty err
        Right val -> Right val

nonEmptyText :: Parser Text -> Parser NonEmptyText
nonEmptyText p = do
    t <- p
    case fromText t of
        Just net -> return net
        Nothing  -> fail "Parser returned empty text"


latinAlpha :: Parser Char
latinAlpha = let isLatinAlpha c = isAsciiLower c || isAsciiUpper c
             in  satisfy isLatinAlpha

latinAlphaNum :: Parser Char
latinAlphaNum = latinAlpha <|> digitChar

-- | Ensures the given prefix is present, then parses the rest.
hasPrefix :: Text -> Parser a -> Parser a
hasPrefix prefix p = string prefix *> p

-- | Fails if 'Nothing' with the given error message.
require :: MonadFail m => String -> Maybe a -> m a
require msg = maybe (fail msg) pure

-- | Fails if the given 'Either' is 'Left'.
requireRight :: MonadFail m => Either Text a -> m a
requireRight = either (fail . T.unpack) pure
