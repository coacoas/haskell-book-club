module Main where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Lib

data Bull = Fools
          | Twoo
          deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Twoo
  mappend Fools Fools = Fools
  mappend Fools Twoo  = Fools
  mappend Twoo Fools  = Fools
  mappend Twoo Twoo   = Twoo

instance EqProp Bull where (=-=) = eq

pair :: Pair Int
pair = Pair 1 2

main :: IO ()
main = do
  quickBatch $ functor pair
  quickBatch $ applicative pair
