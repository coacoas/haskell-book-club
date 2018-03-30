{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter11 where

import Data.Char

data Doggies a = Husky a
               | Mastiff a
  deriving (Eq, Show)

data DogueDeBordeaux doge =
     DogueDeBordeaux doge

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini
                  | Mazda
                  | Tata
  deriving (Eq, Show)

data Airline = PapuAir
             | CatapultsR'Us
             | TakeYourChancesUnited
  deriving (Eq, Show)

data Size = Size Integer deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car man _) = man
getMenu (Plane man _) = man

data Example = MakeExample deriving Show

newtype Goats =
  Goats Int deriving (Eq, Show)

newtype Cows =
  Cows Int deriving (Eq, Show)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (i, _) = i > 42

instance TooMany (Int, Int) where
  tooMany (i, j) = i+j > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (a, b) = tooMany a || tooMany b

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

data BigSmall = Big Bool | Small Bool deriving (Eq, Show)

data FlowerType = Gardenia | Daisy | Rose | Lilac deriving Show
type Gardener = String
data Garden = Garden Gardener FlowerType deriving Show

data OperatingSystem =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)
data ProgLang =
  Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)
data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]
  
allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]
  
allProgrammers :: [Programmer]
allProgrammers = [Programmer opsys language
                 | opsys <- allOperatingSystems,
                   language <- allLanguages]

data Quad = One
          | Two
          | Three
          | Four

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday

g xs = xs !! (length xs - 1)

isSubSeqOf :: Eq a => [a] -> [a] -> Bool
isSubSeqOf sub full = go sub full
  where go [] _ = True
        go _ [] = False
        go subrem@(x:xs) (y:ys) =
          if x == y then go xs ys
          else go subrem ys
           
capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = map capitalize (words sentence)
  where capitalize "" = ("", "")
        capitalize w@(x:xs) = (w, toUpper x : xs)

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs
capitalizeWord "" = ""

split :: Char -> [Char] -> [[Char]]
split delim s = go s []
  where go [] acc = reverse acc
        go remaining acc =
          let cleared = dropWhile (== delim) remaining
              word = takeWhile (/= delim) cleared
              next = drop (length word) cleared
           in go next (word : acc)

sentences :: String -> [String]
sentences = split '.'

trim :: String -> String
trim = dropWhile (==' ')

capitalizeParagraph :: String -> String
capitalizeParagraph paragraph = let
  s = sentences paragraph
  





































