module Unfold where

myIterate :: (a -> a) -> a -> [a]
myIterate f z = z : myIterate f (f z)

myUnfoldr :: (b -> Maybe (a, b))
          -> b
          -> [a]
myUnfoldr f z = go (f z)
  where go Nothing = []
        go (Just (a, z')) = a : go (f z')

betterIterate :: (a -> a) -> a -> [a]
betterIterate f z = myUnfoldr (\a -> Just (a, f a)) z
