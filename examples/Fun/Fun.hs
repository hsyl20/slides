module Main where

-- | Function
{-# NOINLINE add10 #-}
add10 :: Int -> Int
add10 x = x + 10

  -- Try adding a bang to x: push a continuation
  --
  -- With -O2 we can see the +# primop inlined
  --
  -- This example shows that we lack an optimizer

main :: IO ()
main = print $ add10 5
