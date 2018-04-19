module Chapter12 where

import Data.Char
import Data.Maybe

-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe s | s == "the" = Nothing
         | otherwise  = Just s


replaceThe :: String -> String
replaceThe "" = ""
replaceThe (' ':xs) = " " ++ replaceThe xs
replaceThe string = let
  next = takeWhile (/= ' ') string
  rest = drop (length next) $ string
  word = fromMaybe "a" (notThe next)
  in word ++ replaceThe rest

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe' :: String -> String
replaceThe' sentence = let
  ws = words sentence
  transform w | w == "the" = "a"
              | otherwise  = w
  modifieds = map transform ws
  in unwords modifieds


-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel string = go 0 string
  where go :: Integer -> String -> Integer
        go _ "" = 0
        go count (' ':xs) = go count xs
        go count sentence = let
          next = takeWhile (/= ' ') sentence
          vowel = elem (head next) ("aeiouAEIOU" :: [Char])
          in if (vowel) then count
             else let rest = drop (length next) sentence
                      count' = count + (maybe 1 (const 0) $ notThe next)
                      in go count' rest

vowels :: [Char]
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel c = elem (toLower c) vowels


-- >>> countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak"
-- 4
countVowels :: String -> Integer
countVowels s = toInteger . length . (filter isVowel) $ s

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = let vowelCount = countVowels s
               consonantCount = (toInteger . length $ s) - vowelCount
               in if (vowelCount > consonantCount) then Nothing else Just (Word' s)
