module Exercises where

import Control.Monad

data Nope a = NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg


----------------

data PhhhbbtttEither b a = Left' a
                         | Right' b
                         deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap _ (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (<*>) _          (Right' b) = (Right' b)
  (<*>) (Right' b) _          = (Right' b)
  (<*>) (Left' f) (Left' a)   = Left' (f a)

instance Monad (PhhhbbtttEither b) where
  return = pure
  (Right' b) >>= _ = Right' b
  (Left' a) >>= f  = f a

----------------

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a)  = Identity (f a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a


------------------

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append ( Cons x xs) ys = Cons x $ xs `append` ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> as = append (f <$> as) (fs <*> as)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  xs >>= f = join (f <$> xs)


------------------

-- join
j :: Monad m => m (m a) -> m a
j m = m >>= id

-- fmap
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = m >>= (return . f)

-- liftM2
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb

-- (<*>)
a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = do
  a <- ma
  f <- mf
  return (f a)


-- forM
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) f = do
  b <- f a
  bs <- meh as f
  return (b : bs)

-- sequence
flipType :: (Monad m) => [m a] -> m [a]
flipType as = meh as id
