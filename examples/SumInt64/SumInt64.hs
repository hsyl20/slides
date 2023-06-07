module Main where

import Data.Int
import Data.Foldable

foreign import javascript
  "((h,l) => console.log(\"result\", h,l))"
  jsprint
  :: Int64 -> IO ()

go :: Int64 -> Int64 -> Int64
go r x
  | x == 0    = r
  | otherwise = go (r+x) (x-1)

main :: IO ()
main = jsprint (go 0 12345678)
