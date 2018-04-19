module Chapter10 where

import Data.Time
import Data.List

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  ]
  
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr filterDate []
  where filterDate (DbDate time) xs = time : xs
        filterDate _ xs = xs 

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr filterNum []
  where filterNum (DbNumber num) xs = num : xs
        filterNum _ xs = xs 


mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = last . sort . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = let
  filtered = filterDbNumber db
  l = length filtered
  s = foldr (+) 0 filtered
  in (fromIntegral s) / (fromIntegral l)


stops :: String
stops = "pbtdkg"
vowels :: String
vowels = "aeiou"
nouns :: [String]
nouns = ["test", "noun"]
verbs :: [String]
verbs = ["go", "left", "played"]

tuples :: [(Char, Char, Char)]
tuples = [(x, y, z) | x <- stops, y <- vowels, z <- stops]
tuples' :: [(Char, Char, Char)]
tuples' = [('p', y, z) | y <- vowels, z <- stops]
wordTuples :: [(String, String, String)]
wordTuples = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]
tupled :: [a] -> [b] -> [(a, b, a)]
tupled a b = [(x, y, z) | x <- a, y <- b, z <- a]

seekritFunc :: String -> Int
seekritFunc x =
 div (sum (map length (words x)))
     (length (words x))
seekritFuncF :: Fractional a => String -> a
seekritFuncF x =
 (/) (fromIntegral (sum (map length (words x))))
     (fromIntegral (length (words x)))

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\a e -> f a : e) [] xs

squishMap :: [a] -> (a -> [b]) -> [b]
squishMap as f = foldr (\a e -> (f a) ++ e) [] as

mySquish :: [[a]] -> [a]
mySquish = foldr (++) []

squishAgain :: [[a]] -> [a]
squishAgain = flip squishMap id

myMaximumBy :: (a -> a -> Ordering)
               -> [a]
               -> a
myMaximumBy f = head . sortBy f


