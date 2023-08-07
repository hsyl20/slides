module Main where

{-# NOINLINE add10 #-}
add10 :: Int -> Int
add10 x = x + 10

main :: IO ()
main = print $ add10 5
