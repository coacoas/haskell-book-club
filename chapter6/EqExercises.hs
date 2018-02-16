module EqExercises where

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where 
  (==) (TisAn a) (TisAn b) = a == b

-- 

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two x y) = a == x && b == y

--

data StringOrInt =
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAString s) (TisAString t) = s == t
  (==) (TisAnInt   i) (TisAnInt   j) = i == j
  (==) _              _              = False

--

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair c d)  = a == c && b == d

--

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple c d) = a == c && b == d

--

data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne b) = a == b
  (==) (ThatOne a) (ThatOne b) = a == b
  (==) _           _           = False

--

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x)   (Hello y)   = x == y
  (==) (Goodbye x) (Goodbye y) = x == y
  (==) _           _           = False

