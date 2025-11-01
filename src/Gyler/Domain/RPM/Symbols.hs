{-# LANGUAGE OverloadedStrings #-}

module Gyler.Domain.RPM.Symbols (
    separators,
    versionChars,
    releaseChars,
    nameChars,
    rpmChars
) where

-- | Module: Gyler.Domain.RPM.Symbols
--
-- Description: Character sets used across RPM domain entities.

separators :: String
separators = "_.+"

versionChars, releaseChars :: String
versionChars = '~' : separators
releaseChars = versionChars

nameChars :: String
nameChars = '-' : separators

rpmChars :: String
rpmChars = ':' : '-' : '~' : separators
