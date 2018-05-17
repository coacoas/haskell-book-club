module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

mult :: (Integral a) => a -> a -> a
mult x y = go x 0
  where go n acc
         | n == 0     = acc
         | n > 0      = go (n - 1) (acc + y)
         | otherwise  = go (n + 1) (acc - y)

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1 :: Integer) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      (2 + 2 :: Integer) `shouldBe` 4
    it "x + 1 is always\
       \ greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy (15 :: Int) 3 `shouldBe` (5, 0)
    it "22 divided by 5 is\
       \ 4 remainder 2" $ do
      dividedBy (22 :: Integer) 5 `shouldBe` (4, 2)
  describe "Multiplication" $ do
    it "3 * 5 is 15" $ do
      mult 3 5 `shouldBe` (15 :: Integer)
    it "-3 * -5 is 15" $ do
      mult (negate 3) (negate 5) `shouldBe` (15 :: Integer)
    it "3 * -5 is -15" $ do
      mult (3) (negate 5) `shouldBe` (negate 15 :: Integer)
    it "-3 * 5 is -15" $ do
      mult (negate 3) (5) `shouldBe` (negate 15 :: Integer)


prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
