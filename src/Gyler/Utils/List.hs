module Gyler.Utils.List where

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (go xs)
  where
    go [x] = x
    go (_:ys) = go ys
