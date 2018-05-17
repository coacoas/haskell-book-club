module Caesar
  ( caesar, unceasar )
where

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

unceasar :: Int -> [Char] -> [Char]  
unceasar n = ceasar (-n)
