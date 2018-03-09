module Chapter8 where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y
-- fill in the types
flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

sum :: (Num a, Eq a) => a -> a
sum n = go n 0
  where go x acc
         | x == 0    = acc
         | otherwise = go (x - 1) (acc + x)

mult :: (Integral a) => a -> a -> a
mult x y = go x 0
  where go n acc
         | n == 0     = acc
         | n > 0      = go (n - 1) (acc + y)
         | otherwise  = go (n + 1) (acc - y)

data DividedResult = Result Integer | DividedByZero        

-- dividedBy :: Integral a => a -> a -> DividedResult
-- dividedBy num denom
--  | denom == 0 = DividedByZero
--  | denom < 0 =
--      let (q, r) = dividedBy num (negate denom) in 
--        Result (-q)
--  | otherwise = go num denom 0
--          where go n d count
--                 | n < d = (count, n)
--                 | otherwise = go (n + d) d (count + 1)

mc :: Integral a => a -> a
mc n
 | n > 100 = n - 10
 | otherwise = mc . mc $ n + 11
