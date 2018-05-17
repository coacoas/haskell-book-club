module BinaryTree where

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a))
       -> a
       -> BinaryTree b
unfold f z = go $ f z
  where go Nothing = Leaf
        go (Just (l,n,r)) = Node (unfold f l) n (unfold f r)
         

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold go 0
  where go :: Integer -> Maybe (Integer, Integer, Integer)
        go x
         | x == n = Nothing
         | otherwise = Just ((x + 1), x, (x + 1))
