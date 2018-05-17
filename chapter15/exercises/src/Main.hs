module Main where

import Data.Monoid
import Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) Nada = Only a
  mappend (Only a) (Only b) = Only (a <> b)
  mappend Nada other = other

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)  

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a = First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) other = other
  mappend (First' (Only a)) _                = First' (Only a)

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

genFirst' :: Arbitrary a => Gen (First' a)
genFirst' = do
  a <- arbitrary
  frequency [ (1, return (First' Nada))
            , (3, return (First' (Only a)))]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst'

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
