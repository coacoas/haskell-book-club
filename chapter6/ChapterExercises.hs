module ChapterExercises where

import Data.List (sort)

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown x = if x == Woot
               then Blah
               else x


--
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"


data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

phew = Papu (Rocks "chases") (Yeah True)

truth = Papu (Rocks "chomskydoz")
             (Yeah True)

i :: a
i = 1

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

f :: RealFrac a => a
f = 1.0

freud :: Int -> Int
freud x = x

myX = 1 :: Int

sigmund :: Num a => a -> Int
sigmund _ = myX

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f' a b = f'(a) == b

arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith f' _ a = f'(a)

arith f' i a = f'(a) + fromInteger i

arith f' i a = f'(a) ^ i

