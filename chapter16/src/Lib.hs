{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Data.Functor
import GHC.Arr

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = (read . ("123"++)) . show <$> ioi
    in (*3) <$> changed

newtype Identity a = Identity a
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
  
data Two a b = Two a b
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b
instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)
  
data Four' a b = Four' a a a b
instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

-- Can you implement one for this type? Why? Why not?
data Trivial = Trivial
  deriving (Eq)
-- NOPE!  Wrong kind

-- Possibly

data Possibly a = LolNope
                | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

-- Short Exercise

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a

-- Chapter Exercises

data BoolAndSomethingElse a = False' a | True' a
instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a)   = True' (f a)

data BoolAndMaybeSomethingElse a = Falsish | Truish a
instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish    = Falsish
  fmap f (Truish a) = Truish (f a)

newtype Mu f = InF { outF :: f (Mu f) }

data D = D (Array Word Word) Int Int

-- Rearrange the arguments

data Sum' b a = First' a
              | Second' b
instance Functor (Sum' e) where
  fmap f (First' a) = First' (f a)
  fmap f (Second' b) = Second' b

data Company a c b = DeepBlue a c
                   | Something b
instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a
              | R b a b
              deriving (Eq, Show)
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write the functor

data Quant a b = Finance
               | Desk a
               | Bloor b

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)

newtype K a b = K a
instance Functor (K a) where
  fmap f (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
newtype K' a b = K' a
instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip (K' (f a))

newtype EvilGoateeConst a b = GoatyConst b
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

newtype LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
  fmap f' (LiftItOut fa) = LiftItOut (fmap f' fa)
  
data Parappa f g a = DaWrappa (f a) (g a) deriving Show
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f' (DaWrappa fa ga) = DaWrappa (fmap f' fa) (fmap f' ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
  fmap f' (IgnoringSomething f g) = IgnoringSomething f (fmap f' g)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga g) = Notorious go ga (fmap f g)

data List a = Nil | Cons a (List a)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)

data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a)
                            (GoatLord a)
                            (GoatLord a)
instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read r) = Read (fmap f r)
