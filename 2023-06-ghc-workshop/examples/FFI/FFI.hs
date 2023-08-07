{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ExtendedLiterals #-}

module Main where

import GHC.Exts

foreign import javascript
  "((h,l) => { console.log(h,l); })"
  show_w64 :: Word64# -> IO ()

foreign import javascript
  "((x) => {     \
  \  h$ret1 = x * 7; \
  \  return x * 3;   \
  \})"
  foo :: Word32# -> Word64#

main = do
  show_w64 0x00000001_00000002#Word64
  show_w64 (foo 6#Word32)
