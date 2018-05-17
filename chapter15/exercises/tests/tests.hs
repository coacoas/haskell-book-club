module Main where

import Data.Semigroup
import Test.QuickCheck
import ChapterExercises

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (mappend a (mappend b c)) == (mappend (mappend a b) c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mappend mempty a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mappend a mempty) == a

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
type TrivIdentity = Trivial -> Bool
type IdentityAssoc a = (Identity a) -> (Identity a) -> (Identity a) -> Bool
type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool
type ThreeAssoc a b c = (Three a b c) -> (Three a b c) -> (Three a b c) -> Bool

type OrAssoc a b = (Or a b) -> (Or a b) -> (Or a b) -> Bool

trivialMonoidProperties :: IO ()
trivialMonoidProperties = do
  quickCheck (monoidAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: TrivIdentity)
  quickCheck (monoidRightIdentity :: TrivIdentity)

identityMonoidProperties :: IO ()
identityMonoidProperties = do
  quickCheck (monoidAssoc :: IdentityAssoc (Product Int))
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity [Double] -> Bool)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc (Sum Int))
  quickCheck (semigroupAssoc :: TwoAssoc (Product Integer) String)
  quickCheck (semigroupAssoc :: ThreeAssoc [Double] (Product Integer) String)
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: OrAssoc [String] (Product Integer))
  trivialMonoidProperties
  identityMonoidProperties
