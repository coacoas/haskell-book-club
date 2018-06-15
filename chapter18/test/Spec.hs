import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Exercises

data CountMe a = CountMe Integer a
               deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a =
    CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe n' b = f a
    in CountMe (n + n') b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = oneof [left, right] where
    left = Left' <$> arbitrary
    right = Right' <$> arbitrary

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq


instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [empty, cons] where
    empty = return Nil
    cons  = Cons <$> arbitrary <*> arbitrary

instance Eq a => EqProp (List a) where
  (=-=) = eq

  

main :: IO ()
main = do
  let triggerList :: List (Char, String, Int)
      triggerList = undefined  
  quickBatch $ functor triggerList
  quickBatch $ applicative triggerList
  quickBatch $ monad triggerList

  let triggerIdentity :: Identity (Char, String, Int)
      triggerIdentity = undefined  
  quickBatch $ functor triggerIdentity
  quickBatch $ applicative triggerIdentity
  quickBatch $ monad triggerIdentity

  let triggerPhhhbbtttEither :: PhhhbbtttEither (Char, String, Int) (Int, Char, String)
      triggerPhhhbbtttEither = undefined  
  quickBatch $ functor triggerPhhhbbtttEither
  quickBatch $ applicative triggerPhhhbbtttEither
  quickBatch $ monad triggerPhhhbbtttEither

  let trigger :: Nope (Int, Char, String)
      trigger = undefined  
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
