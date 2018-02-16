module Exercises3_5 where

ex1 :: [Int]
-- WAS: ++ [1, 2, 3] [4, 5, 6]
-- Need () to make it prefix function
ex1 = (++) [1, 2, 3] [4, 5, 6]

ex2 :: String
-- WAS: '<3' ++ ' Haskell'
-- Can't (++) Char
ex2 = "<3" ++ " Haskell"

ex3 :: String
-- Works as published
ex3 = concat ["<3", " Haskell"]
