module Gyler.Types (
    Exec, Args, Cmd
) where

import Gyler.Data.NonEmptyText (NonEmptyText)

type Exec = NonEmptyText
type Args = [NonEmptyText]
type Cmd  = (Exec, Args)
