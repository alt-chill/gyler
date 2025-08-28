module Gyler.Utils.Maybe (
    rightToMaybe
) where

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)  = Nothing
rightToMaybe (Right x) = Just x
