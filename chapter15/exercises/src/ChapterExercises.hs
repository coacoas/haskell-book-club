module ChapterExercises where

import Data.Semigroup
import Test.QuickCheck

-- Trivial

data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)
  
instance Arbitrary Trivial where
  arbitrary = return Trivial

-- Identity

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)
  
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)
  
-- Two

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)
  
--- Three

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)
  
-- Skipping Four

-- BoolConj

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj b1) <> (BoolConj b2) = BoolConj (getAll $ All b1 <> All b2)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return (BoolConj b)

-- BoolConj

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj b1) <> (BoolDisj b2) = BoolDisj (getAny $ Any b1 <> Any b2)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = do
    b <- arbitrary
    return (BoolDisj b)

-- Or

data Or a b = Fst a
            | Snd b
            deriving (Eq, Show)

instance Semigroup (Or a b) where
  _ <> Snd b = Snd b
  Snd b <> _ = Snd b
  Fst _ <> Fst a2 = Fst a2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [ return $ Fst a
          , return $ Snd b
          ]

-- Combine

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine (const mempty)
  mappend (Combine f) (Combine g) = Combine (\a -> mappend (f a) (g a))

-- Comp
newtype Comp a = Comp (a -> a)

instance Monoid (Comp a) where
  mempty = Comp id
  mappend (Comp f) (Comp g) = Comp (f . g)

-- Mem (State?)

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance (Monoid a) => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
  mappend (Mem f) (Mem g) = Mem
    (\s -> 
       let
         (a1, s1) = f s
         (a2, s2) = g s1
       in (mappend a1 a2, s2))
