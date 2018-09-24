module DList where

import Criterion.Main

newtype DList a = DL { unDL :: [a]->[a] }

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton = DL . (:)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList = ($ []) . unDL
{-# INLINE toList #-}

infixr `cons`
cons      :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

-- Append a single element to a dlist.
infixl `snoc`
snoc      :: DList a -> a -> DList a
-- snoc xs x = append xs (singleton x)
snoc xs x = DL $ unDL xs . (x:)
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append xs ys = DL $ (unDL xs) . (unDL ys)
{-# INLINE append#-}

