module Chapter9 where

import Data.Char
import Data.List

eftBool :: Bool -> Bool -> [Bool]
eftBool start stop = go stop []
  where go current acc
         | current < start = reverse $ eftBool stop start
         | current == start = current : acc
         | otherwise = go (pred current) (current : acc)

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd start stop = go stop []
  where go current acc
         | current < start = reverse $ eftOrd stop start
         | current == start = current : acc
         | otherwise = go (pred current) (current : acc)

eftInt :: Int -> Int -> [Int]
eftInt start stop = go stop []
  where go current acc
         | current < start = reverse $ eftInt stop start
         | current == start = current : acc
         | otherwise = go (pred current) (current : acc)

eftChar :: Char -> Char -> [Char]
eftChar start stop = go stop []
  where go current acc
         | current < start = reverse $ eftChar stop start
         | current == start = current : acc
         | otherwise = go (pred current) (current : acc)

myWords :: Char -> [Char] -> [[Char]]
myWords delim s = go s []
  where go [] acc = reverse acc
        go remaining acc =
          let cleared = dropWhile (== delim) remaining
              word = takeWhile (/= delim) cleared
              next = drop (length word) cleared
           in go next (word : acc)


mySqr :: Integral a => [a]
mySqr = [x^2 | x <- [1..10]]

mySqr' :: Integral a => [a]
mySqr' = [x^2 | x <- [1..5]]

myCube :: Integral a => [a]
myCube = [y^3 | y <- [1..5]]

myTuples :: Integral a => [(a,a)]
myTuples = [(x, y) | x <- mySqr', y <- myCube]

myTuples' :: Integral a => [(a,a)]
myTuples' = [(x, y) | x <- mySqr', x < 50, y <- myCube, y < 50]

myFilter    :: [Char] -> [[Char]]
myFilter xs = filter notArticles wordList
  where articles = ["a", "an", "the"]
        notArticles x = not $ elem x articles
        wordList = myWords ' ' xs

zip' :: [a] -> [b] -> [(a, b)]
zip' (x:xs) (y:ys) = ((x,y) : zip' xs ys)
zip' _      _      = []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = ((f x y) : zipWith' f xs ys)
zipWith' _ _      _      = []

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith (,) 

-- Chapter Exercises

justUpper :: [Char] -> [Char]
justUpper = filter isUpper

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (x:xs) = (toUpper x) : xs

toUpperCase :: [Char] -> [Char]
toUpperCase [] = []
toUpperCase (x:xs) = (toUpper x) : toUpperCase xs

firstAsCap :: [Char] -> Char
firstAsCap = toUpper . head



myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem a = myAny (== a)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "No maximum on empty list"
myMaximumBy o (a:as) = go as a
  where go [] acc = acc
        go (x: xs) acc
          | o x acc == GT = go xs x
          | otherwise     = go xs acc

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "No minimum on empty list"
myMinimumBy o (a:as) = go as a
  where go [] acc = acc
        go (x: xs) acc
          | o x acc == LT = go xs x
          | otherwise     = go xs acc

myMaximum :: (Ord a) => [a] -> a
myMaximum (a:as) = go as a
  where go [] acc = acc
        go (x: xs) acc
          | compare x acc == GT = go xs x
          | otherwise     = go xs acc

myMinimum :: (Ord a) => [a] -> a
myMinimum (a:as) = go as a
  where go [] acc = acc
        go (x: xs) acc
          | compare x acc == LT = go xs x
          | otherwise     = go xs acc

