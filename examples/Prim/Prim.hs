{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Data.Word
import GHC.Exts
import GHC.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

bar :: (# #) -> (# Word32#, Word64#, Float#, Addr# #)
bar _ = (# 32#Word32, 64#Word64, 17.0#, nullAddr# #)


main :: IO ()
main = do
  -- initialize 2 ByteArrays
  x <- mallocBytes 4
  y <- mallocBytes 4
  poke x (4 :: Word32)
  poke y (8 :: Word32)

  -- show their "address"
  print (x,y)

  -- peek and show the values
  print =<< (,) <$> peek x <*> peek y

  -- JS dump to the rescue
  -- js_log x
  -- print (x `plusPtr` 7)
  -- js_log (x `plusPtr` 7)

foreign import javascript
  "((a,o) => h$log({ addr: a, offset: o}))"
  js_log :: Ptr Word32 -> IO ()

