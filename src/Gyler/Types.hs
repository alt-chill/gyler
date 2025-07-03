module Gyler.Types (
    Exec, Args, Cmd
) where

import Data.Text (Text)

type Exec = Text
type Args = [Text]
type Cmd  = (Exec, Args)
