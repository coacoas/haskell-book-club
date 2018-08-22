{-# LANGUAGE InstanceSigs #-}
module Monad where

import Lib
import Applicative
import Control.Monad
import Data.Foldable

foo :: (Functor f,Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r,length t)

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> let
    a = ra r
    b = aRb a
    in (runReader b) r

getDogRM :: Reader Person Dog
getDogRM = Reader $ do
  name <- dogName
  addy <- address
  return $ Dog name addy
