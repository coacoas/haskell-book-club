module Vigenère where

import Data.Char

-- Assumptions:
-- lowercase only
-- no spaces

vigenereChar :: (Char, Char) -> Char
 (cipherChar, messageChar) = let
    a = ord 'a'
    mc = (ord messageChar) - a
    cc = (ord cipherChar) - a
  in chr $ ((mc + cc) `mod` 26) + a
      
 :: String -> String -> String
 key message= let
          cipher = concat $ repeat key
          zipped = zip cipher message
 map vigenereChar zipped

 :: (Char, Char) -> Char
 (cipherChar, messageChar) = let
    a = ord 'a'
    mc = (ord messageChar) - a
    cc = (ord cipherChar) - a
  in chr $ ((mc - cc) `mod` 26) + a

 :: String -> String -> String
 key message = let
    cipher = concat $ repeat key
    zipped = zip cipher message
 map unvigenereChar zipped

