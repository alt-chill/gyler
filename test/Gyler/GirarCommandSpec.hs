{-# LANGUAGE OverloadedStrings #-}

module Gyler.GirarCommandSpec (spec) where

import Test.Hspec
import Gyler.GirarCommand
import Gyler.Context
import Gyler.Types
import Gyler.Data.NonEmptyText.Unsafe

import Control.Lens
import Data.Text (Text)

spec :: Spec
spec = describe "toCmd" $ do
    let fullSshCfg = defSshConfig
            & sshExecutable .~ "ssh"
            & sshArgs       .~ ["-v"]
            & remoteUser    .~ "alice"
            & remoteHost    .~ "example.com"
            & remotePort    ?~ "2222"
            & authKey       ?~ "/path/to/key"

        minimalSshCfg = defSshConfig
            & remoteUser .~ "user"
            & remoteHost .~ "host"
            -- remotePort and authKey remain Nothing

        fullCurlCfg = defCurlConfig
            & curlExecutable .~ "curl"
            & curlArgs       .~ ["-f"]

        emptyCurlCfg = defCurlConfig
            & curlArgs .~ []

        cfgFull = defCommandsConfig
            & gyleSsh   ?~ fullSshCfg
            & giterySsh ?~ fullSshCfg
            & girarWeb  ?~ fullCurlCfg

        cfgMinimal = defCommandsConfig
            & gyleSsh   ?~ minimalSshCfg
            & giterySsh ?~ minimalSshCfg
            & girarWeb  ?~ emptyCurlCfg

        cfgEmpty = defCommandsConfig
            & gyleSsh   .~ Nothing
            & giterySsh .~ Nothing
            & girarWeb  .~ Nothing

    describe "ViaGyle" $ do
        it "constructs full ssh command with args, port, key" $ do
            let cmd = toCmd cfgFull (ViaGyle ["do", "thing"])
            cmd `shouldBe` Right ("ssh", ["-v", "-p", "2222", "-i", "/path/to/key", "alice@example.com", "do", "thing"])

        it "uses default port 22 and no key when not specified" $ do
            let cmd = toCmd cfgMinimal (ViaGyle [])
            cmd `shouldBe` Right ("ssh", ["-p", "22", "user@host"])

        it "works with empty args" $ do
            let cmd = toCmd cfgFull (ViaGyle [])
            cmd `shouldBe` Right ("ssh", ["-v", "-p", "2222", "-i", "/path/to/key", "alice@example.com"])

    describe "ViaGitery" $ do
        it "uses same ssh construction logic as ViaGyle" $ do
            let cmd = toCmd cfgFull (ViaGitery ["task", "ls"])
            cmd `shouldBe` Right ("ssh", ["-v", "-p", "2222", "-i", "/path/to/key", "alice@example.com", "task", "ls"])

        it "works with empty args" $ do
            let cmd = toCmd cfgMinimal (ViaGitery [])
            cmd `shouldBe` Right ("ssh", ["-p", "22", "user@host"])

    describe "ViaCurl" $ do
        it "constructs curl command with args and target" $ do
            let cmd = toCmd cfgFull (ViaGirarWeb "https://git.altlinux.org/people")
            cmd `shouldBe` Right ("curl", ["-f", "https://git.altlinux.org/people"])

        it "works without curl extra args" $ do
            let cmd = toCmd cfgMinimal (ViaGirarWeb "https://git.altlinux.org/people")
            cmd `shouldBe` Right ("curl", ["https://git.altlinux.org/people"])

    describe "Missing config cases" $ do
        it "returns error when gyleSsh config is missing" $ do
            let cmd = toCmd cfgEmpty (ViaGyle ["something"])
            cmd `shouldBe` Left "toCmd (ViaGyle): SshConfig is not available"

        it "returns error when giterySsh config is missing" $ do
            let cmd = toCmd cfgEmpty (ViaGitery ["task"])
            cmd `shouldBe` Left "toCmd (ViaGitery): SshConfig is not available"

        it "returns error when girarWeb config is missing" $ do
            let cmd = toCmd cfgEmpty (ViaGirarWeb "https://missing.config")
            cmd `shouldBe` Left "toCmd (ViaGirarWeb): CurlConfig is not available"
