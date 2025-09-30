{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TestUtils.FetchSpec (
  genOutput,
  encodeAndWrite
  ) where

import qualified Data.ByteString as BS
import           Data.Serialize
import           Gyler.FetchSpec (FetchSpec(..))
import           System.FilePath

-- | Generate a golden file for given Serializable data.
--
-- Intended to be used with `mkFetchSpecTest` from `TestUtils.FetchSpec.Template`
--
-- Example usage in GHCi:
--
-- >>> set = mkValidSet $ mapMaybe NET.fromText ["respublica", "sin", "at"] :: MaintainersSet
-- >>> encodeAndWrite set "test/golden/FetchSpec/Maintainers/output.dat"
--
encodeAndWrite :: (Serialize e) => e -> FilePath -> IO ()
encodeAndWrite dat file = do
        BS.writeFile file (encode dat)
        putStrLn $ "Golden file written to " <> file

-- | Generate an output.dat golden file for a given FetchSpec.
--
-- Function reads input from text file and saves result to "output.dat" in the
-- same dir with input file.
--
-- Intended to be used with `mkFetchSpecTest` from `TestUtils.FetchSpec.Template`
--
-- Example usage in GHCi:
--
-- >>> genOutputTxt myFetchSpec "test/golden/FetchSpec/MySpec/input.txt"
--
-- Result will be saved in "test/golden/FetchSpec/MySpec/output.dat"
--
genOutput :: (FetchSpec e, Show e) => e -> FilePath -> IO ()
genOutput spec inputPath = do
    let outputPath = inputPath `replaceFileName` "output.dat"

    putStrLn $ "Output: " <> outputPath

    input <- BS.readFile inputPath
    case parseResult spec Nothing input of
        Left err     -> putStrLn $ "Parse failed: " <> show err
        Right result -> encodeAndWrite result outputPath
