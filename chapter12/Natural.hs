module Natural where

import Data.Maybe

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger n = go 0 n
  where go :: Integer -> Nat -> Integer
        go acc Zero = acc
        go acc (Succ nat) = go (acc + 1) nat

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0     = Nothing
  | i == 0    = Just Zero
  | otherwise = Just . Succ . fromJust . integerToNat $ i - 1
  -- or fmap Succ (integerToNat (i - 1))
