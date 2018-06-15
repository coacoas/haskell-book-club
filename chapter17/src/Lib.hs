module Lib where

import Control.Applicative
import Data.Monoid

f x = lookup x [ (3, "hello")
               , (4, "julie")
               , (5, "kbai")]
g y = lookup y [ (7, "sup?")
               , (8, "chris")
               , (9, "aloha")]
h z = lookup z [(2, 3), (5, 6), (7, 8)]
om x = lookup x [(4, 10), (8, 13), (1, 9001)]

-----------------------------

newtype Identity a = Identity a
                   deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

-----------------------------

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant x) (Constant y) = Constant (mappend x y)

-----------------------------

data List a = Nil
            | Cons a ( List a)
            deriving ( Eq , Show )

append :: List a -> List a -> List a
append Nil ys = ys
append ( Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b ( Cons h t) = f h (fold f b t)

concat' :: List ( List a) -> List a
concat' = fold append Nil

-- write this one in terms
-- of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ f <$> as

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Applicative List where
  pure  a               = Cons a (Nil)
  (<*>) _ Nil           = Nil
  (<*>) Nil _           = Nil
  (<*>) (Cons f fs) l   = append (f <$> l) (fs <*> (l))

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

newtype ZipList' a = ZipList' ( List a) deriving ( Eq , Show )

instance Functor ZipList' where
  fmap f ( ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure  x = ZipList' (Cons x Nil)
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons x xs))  = ZipList' (Cons (f x) (fs <*> xs))
  

--------------------------

data Validation e a = Failure e
                    | Success a
                    deriving ( Eq , Show )

-- same as Either
instance Functor ( Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) _ (Failure e) = Failure e
  (<*>) (Failure f) _ = Failure f
  (<*>) (Success f) (Success a) = Success (f a)
  
---------------------------

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f1 f2) (Pair a1 a2) = Pair (f1 a1) (f2 a2)
  
