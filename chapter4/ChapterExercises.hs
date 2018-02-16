{-#LANGUAGE RankNTypes #-}

module ChapterExercises where

awesome :: [String]
awesome = ["Papuchon", "curry", ":)"]

alsoAwesome :: [[Char]]
alsoAwesome = ["Quake", "The Simons"]

allAwesome :: [[String]]
allAwesome = [awesome, alsoAwesome]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

myAbs :: Integer -> Integer
myAbs a = if (a < 0) then -a else a

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f ab cd = ((snd ab, snd cd), (fst ab, fst cd))

-- Syntax corrections

x' :: Int -> Int -> Int
x' = (+)

-- Fix lowercase function name
-- Fix single quote to backtick
f' :: [a] -> Int
f' xs = w `x'` 1
  where w = length xs

-- Fix lowercase argument name
-- Fix arrow
id' :: a -> a
id' = \a -> a

-- Parentheses around deconstructed parameter
-- Underscore so it doesn't complain about an unused value
head :: [a] -> a
head = \(x:xs) -> x

-- Add comma
-- Lowercase return value 
first :: forall a b. ((a,b) -> a)
first (a, _) = a

