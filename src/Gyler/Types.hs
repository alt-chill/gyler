{-# LANGUAGE OverloadedStrings #-}

module Gyler.Types (
    Exec, Args, Cmd,
    showCmd
) where

import Data.Text (Text, intercalate)

import Gyler.Data.NonEmptyText (NonEmptyText, toText)


type Exec = NonEmptyText
type Args = [NonEmptyText]
type Cmd  = (Exec, Args)

showCmd :: Cmd -> Text
showCmd (a, b) = mconcat ["(", toText a, ", [", (intercalate ", " . fmap toText) b, "])"]
