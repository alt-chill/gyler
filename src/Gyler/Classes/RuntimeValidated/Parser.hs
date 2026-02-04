{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Gyler.Classes.RuntimeValidated.Parser (
    runtimeValidatedParser,
    runtimeValidatedParserM
) where

-- | Module: Gyler.Classes.RuntimeValidated.Parser
--
-- Description: A universal abstract parser for `RuntimeValidated` values.
--
-- To use this module, a `RuntimeValidated` value must have an instance of the
-- `HasParsableRaw` typeclass, and an environment providing a `HasValidSetFor`
-- instance must be available.
--
-- To create a parser, pass the environment to the `runtimeValidatedParser`
-- function. It returns a parser for a `RuntimeValidated` value, which can be
-- composed with other parser combinators.

import Gyler.Parsers.Type (Parser)

import Gyler.Data.ValidContainer (ValidContainer(vcToList))

import Gyler.Classes.RuntimeValidated (RuntimeValidated(Raw))
import Gyler.Classes.RuntimeValidated.Environment (HasValidSetFor(..), validateRawFromEnv)
import Gyler.Classes.RuntimeValidated.Parser.Raw (HasParsableRaw(..))

import Data.Proxy (Proxy(..))
import Data.List  (intercalate)

import Control.Monad.Reader (MonadReader, asks)

-- | Creates a parser for RuntimeValidated value using the provided environment.
runtimeValidatedParser :: forall rv e c. (
        HasValidSetFor rv e,
        HasParsableRaw rv,

        ValidSet rv e ~ c rv,
        ValidContainer c,

        Show (Raw rv),
        Show rv,
        Show (ValidSet rv e))
        => e                -- ^ Environment containing the valid set
        -> Parser rv        -- ^ Resulting parser
runtimeValidatedParser env = do
    raw <- rawParser (Proxy :: Proxy rv)
    case validateRawFromEnv env raw of
        Just val -> return val
        Nothing  -> fail $ mconcat ["RuntimeValidatedParser: Value '", show raw, "' is not in ValidSet.",
                                    "\nWhich consist of ", showListEllipsis 5 . vcToList $ (getValidSet env :: ValidSet rv e)]

    where -- Pretty printed list: ["cas", "zak", "res", ...]
          showListEllipsis :: Show a => Int -> [a] -> String
          showListEllipsis n xs =
            case splitAt n xs of
                (prefix, []) -> show prefix
                (prefix, _ ) -> "[" ++ intercalate ", " (map show prefix) ++ ", ...]"

runtimeValidatedParserM :: forall rv e c m. (
        HasValidSetFor rv e,
        HasParsableRaw rv,

        ValidSet rv e ~ c rv,
        ValidContainer c,

        Show (Raw rv),
        Show rv,
        Show (ValidSet rv e),

        MonadReader e m)
        => m (Parser rv)
runtimeValidatedParserM = asks runtimeValidatedParser
