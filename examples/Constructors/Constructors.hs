module Main where

-- | Arity 0 data constructor
nothing :: Maybe Int
nothing = Nothing

-- | Arity 1 data constructor
just5 :: Maybe Integer
just5 = Just 5

-- | CAF
someCAF :: Integer
someCAF = sum [0..10]

main :: IO ()
main = do
  -- dumpCAF
  print someCAF
  -- dumpCAF

foreign import javascript
  "(() => { h$log(h$mainZCMainzisomeCAF);})"
  dumpCAF :: IO ()
