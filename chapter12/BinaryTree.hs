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
treeBuild = unfold go
  where go :: Integer -> Maybe (Integer, Integer, Integer)
        go 0 = Nothing
        go n = Just ((n - 1), n, (n - 1))
