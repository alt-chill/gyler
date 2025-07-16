module Gyler.GylerM (
    GylerM,
    runGylerM,
) where

import Control.Monad.Reader (ReaderT, runReaderT)

import Gyler.Context.Gyler (GylerContext)

type GylerM = ReaderT GylerContext IO

runGylerM :: GylerContext -> GylerM a -> IO a
runGylerM = flip runReaderT
