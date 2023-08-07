{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Svg
import Foreign.C.String

svg :: Element -> Element
svg content =
     doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "482", Height_ <<- "340"]

logo :: Element
logo =
     path_ [ Fill_ <<- "#352950"
           , D_ <<- ( mA 0 340 <> lA 113 170 <> lA 0 0 <> lA 85 0
                   <> lA 198 170 <> lA 85 340 <> lA 0 340 <> z <> mA 0 340 ) ]
  <> path_ [ Fill_ <<- "#4A3A74"
           , D_ <<- ( mA 113 340 <> lA 226 170 <> lA 113 0 <> lA 198 0
                   <> lA 425 340 <> lA 340 340 <> lA 269 234 <> lA 198 340
                   <> lA 113 340 <> z <> mA 113 340 ) ]
  <> path_ [ Fill_ <<- "#7C3679"
           , D_ <<- ( mA 387 241 <> lA 350 184 <> lA 482 184 <> lA 482 241
                   <> lA 387 241 <> z <> mA 387 241 ) ]
  <> path_ [ Fill_ <<- "#7C3679"
           , D_ <<- ( mA 331 156 <> lA 293 99 <> lA 482 99 <> lA 482 156
                   <> lA 331 156 <> z <> mA 331 156 ) ]

main :: IO ()
main = do
  let img = show (svg logo)
  print img


  -- withCString img setBody

-- foreign import javascript unsafe
--   "((str_a,str_o) => { document.body.innerHTML = h$decodeUtf8z(str_a,str_o); })"
--   setBody :: CString -> IO ()
