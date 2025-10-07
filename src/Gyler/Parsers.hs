-- |
-- Module: Gyler.Parsers
-- Description: basic parsers combinators, built on top of `Megaparsec`

module Gyler.Parsers (
    Parser(..),
    useParser,
    lexeme,
    symbol,
    word,

    nonEmptyText
) where

import Data.Text (Text)
import Data.Void (Void)

import qualified Data.Text as T (pack)

import Text.Megaparsec (Parsec, some, (<|>), empty, parse, eof, errorBundlePretty)
import Text.Megaparsec.Char (space1, alphaNumChar, markChar)
import qualified Text.Megaparsec.Char.Lexer as L (lexeme, symbol, space)

import Gyler.Data.NonEmptyText (NonEmptyText, fromText)

type Parser = Parsec Void Text

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
