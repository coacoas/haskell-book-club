module Lib where

import Data.Monoid

---------------------------------

sum :: (Foldable t, Num a) => t a -> a
sum ta = getSum $ foldMap Sum ta

---------------------------------

product :: (Foldable t, Num a) => t a -> a
product ta = getProduct $ foldMap Product ta

---------------------------------

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a ta = foldr f False ta where
  f elem found = found || (elem == a)

---------------------------------

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum ta = foldr f Nothing ta where
  f elem Nothing = Just elem
  f elem min@(Just a) = if a < elem then min else (Just elem)

---------------------------------

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum ta = foldr f Nothing ta where
  f elem Nothing = Just elem
  f elem max@(Just a) = if a > elem then max else (Just elem)

---------------------------------

null :: (Foldable t) => t a -> Bool
null = foldr f True where
  f _ _ = False

---------------------------------

length :: (Foldable t) => t a -> Int
length ta = getSum $ foldMap (const (1 :: Sum Int)) ta

---------------------------------

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

---------------------------------

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f ta = foldr f' mempty ta where
  f' elem acc = (f elem) <> acc

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id
