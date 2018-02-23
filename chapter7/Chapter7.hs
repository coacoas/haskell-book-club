module Chapter7 where

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100

numbers :: (Num a, Ord a) => a -> Integer
numbers x
  | x < 0     = -1
  | x > 0     = 1
  | otherwise = 0

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where (xLast, _) = divMod x 10
        (_, d) = divMod xLast 10

xDigit :: Integral a => a -> a -> a
xDigit x d = d' 
  where xLast = div x d
        d'    = mod xLast 10

xDigit' :: Integral a => a -> a -> a
xDigit' d x = d' 
  where xLast = div x (10 ^ d)
        d'    = mod xLast 10
tensDigit'' = xDigit' 1
hundredsDigit'' = xDigit' 2

foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
  False -> x
  True -> y

foldBoolG :: a -> a -> Bool -> a
foldBoolG x y b
  | b     = y
  | not b = x

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

roundTripAB :: (Show a, Read b) => a -> b
roundTripAB = read . show

main :: IO ()
main = do
  print ((roundTripAB (4 :: Integer)) :: Float)
