{-# LANGUAGE OverloadedStrings #-}

module Gyler.GirarCommandSpec (spec) where

import Test.Hspec
import Gyler.GirarCommand
import Gyler.Context
import Gyler.Types

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
            & curlArgs       .~ ["-X", "POST"]

        emptyCurlCfg = defCurlConfig
            & curlArgs .~ []

        cfgFull = defCommandsConfig
            & gyleConfig   .~ fullSshCfg
            & giteryConfig .~ fullSshCfg
            & curlConfig   .~ fullCurlCfg

        cfgMinimal = defCommandsConfig
            & gyleConfig   .~ minimalSshCfg
            & giteryConfig .~ minimalSshCfg
            & curlConfig   .~ emptyCurlCfg

    describe "ViaGyle" $ do
        it "constructs full ssh command with args, port, key" $ do
            let cmd = toCmd cfgFull (ViaGyle ["--do", "thing"])
            cmd `shouldBe` ("ssh", ["-v", "alice@example.com", "-p", "2222", "-i", "/path/to/key", "--do", "thing"])

        it "uses default port 22 and no key when not specified" $ do
            let cmd = toCmd cfgMinimal (ViaGyle ["--fast"])
            cmd `shouldBe` ("ssh", ["user@host", "-p", "22", "--fast"])

        it "works with empty args" $ do
            let cmd = toCmd cfgFull (ViaGyle [])
            cmd `shouldBe` ("ssh", ["-v", "alice@example.com", "-p", "2222", "-i", "/path/to/key"])

    describe "ViaGitery" $ do
        it "uses same ssh construction logic as ViaGyle" $ do
            let cmd = toCmd cfgFull (ViaGitery ["--pull"])
            cmd `shouldBe` ("ssh", ["-v", "alice@example.com", "-p", "2222", "-i", "/path/to/key", "--pull"])

        it "works with empty args" $ do
            let cmd = toCmd cfgMinimal (ViaGitery [])
            cmd `shouldBe` ("ssh", ["user@host", "-p", "22"])

    describe "ViaCurl" $ do
        it "constructs curl command with args and target" $ do
            let cmd = toCmd cfgFull (ViaCurl "https://api.site.com/refresh")
            cmd `shouldBe` ("curl", ["-X", "POST", "https://api.site.com/refresh"])

        it "works with empty curl args" $ do
            let cmd = toCmd cfgMinimal (ViaCurl "http://simple.com/")
            cmd `shouldBe` ("curl", ["http://simple.com/"])
