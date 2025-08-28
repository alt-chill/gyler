{-# LANGUAGE OverloadedStrings #-}

module Gyler.FetchSpec.MaintainersQuerySpec (spec) where

import Test.Hspec

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Gyler.Data.NonEmptyText as NET

import qualified Data.ByteString as BS

import Gyler.FetchSpec
import Gyler.FetchSpec.MaintainersQuery

import Gyler.Context

import Gyler.Data.ValidContainer.HashSet (toList)

import Gyler.Classes.RuntimeValidated

import Control.Lens

import Gyler.Domain.Maintainer

spec :: Spec
spec = describe "Gyler.FetchSpec.MaintainersQuery" $ do
    it "Maintainers parseValue works as expected" $ do
        input  <- BS.readFile "test/golden/FetchSpec/Maintainers/input.txt"

        Just expected <- fmap NET.lines . NET.fromText <$> TIO.readFile "test/golden/FetchSpec/Maintainers/output.txt"

        let Just (MaintainersSet parsed) = parseResult MaintainersQuery Nothing input

            netList = getRaw <$> toList parsed

        netList `shouldMatchList` expected
