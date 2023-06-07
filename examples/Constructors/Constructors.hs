module Main where

-- | Arity 0 data constructor
nothing :: Maybe Int
nothing = Nothing

-- | Arity 1 data constructor
just5 :: Maybe Integer
just5 = Just 5

-- | CAF
someCAF :: Integer
someCAF = sum [0..]

main :: IO ()
main = return ()
