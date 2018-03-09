module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "how did you get this number?"

digits :: Int -> [Int]
digits n = go n []
  where go x acc
          | x < 10 = x : acc
          | otherwise = let (next, digit) = divMod x 10
                        in go next (digit : acc)

wordNumber :: Int -> String
wordNumber n =
  let ds = digits n
      words = map digitToWord ds
      grouped = intersperse "-" words
  in concat grouped
