module Main where

import Data.Int

main :: IO ()
main = print (foo 0 12345678)

foo :: Int64 -> Int64 -> Int64
foo r x
  | x == 0    = r
  | otherwise = foo (r+x) (x-1)
