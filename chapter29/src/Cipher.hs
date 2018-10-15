module Cipher ( vignere
              , encrypt
              , decrypt
              ) where

import Data.Char

caesar :: Int -> [Char] -> [Char]
caesar n s = map (shift) s
  where
    a = ord 'a'
    ciphered c = (mod (ord c + n - a) 26 + a)
    shift = chr . ciphered
        

unCaesar :: Int -> [Char] -> [Char]
unCaesar n s = caesar (negate n) s

ceasarCharLower :: Int -> Char -> Char
ceasarCharLower n c = chr ( ((ord c) - (ord 'a') + n) `mod` 26 + (ord 'a') )

ceasarCharUpper :: Int -> Char -> Char
ceasarCharUpper n c = toUpper ((ceasarCharLower n) (toLower c))

ceasarChar :: Int -> Char -> Char
ceasarChar n c
  | c >= 'a' && c <= 'z' = ceasarCharLower n c
  | c >= 'A' && c <= 'Z' = ceasarCharUpper n c
  | otherwise            = c

ceasar :: Int -> [Char] -> [Char]  
ceasar n = map (ceasarChar n)

unCeasar :: Int -> [Char] -> [Char]  
unCeasar n = ceasar (-n)

zipConditionally :: (a -> Bool) -> [a] -> [a] -> [(a, a)]
zipConditionally _ [] _ = []
zipConditionally _ _ [] = []
zipConditionally p (x:xs) (y:ys) =
  if (p x) then (x,y):(zipConditionally p xs ys)
  else (x,x):(zipConditionally p xs (y:ys))

vignere :: (Int -> Int) -> String -> String -> String
vignere f key message = fmap go aligned
  where aligned = zipConditionally isLetter message (cycle key)
        go (msgChar, keyChar) =
          let diff = f $ (ord $ toLower keyChar) - (ord 'a')
          in ceasarChar diff msgChar

encrypt :: String -> String -> String
encrypt = vignere id

decrypt :: String -> String -> String
decrypt = vignere negate
