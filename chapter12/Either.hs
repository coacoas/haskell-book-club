module Either where

lefts' :: [Either a b] -> [a]
lefts' eithers = let
  justLefts (Left x) = [x]
  justLefts (Right _) = []
  in foldr (\e -> \acc -> justLefts e ++ acc) [] eithers

lefts'' :: [Either a b] -> [a]
lefts'' = fst . partitionEithers''

rights' :: [Either a b] -> [b]
rights' eithers = let
  justRights (Left _)  = []
  justRights (Right x) = [x]
  in foldr (\e -> \acc -> justRights e ++ acc) [] eithers

rights'' :: [Either a b] -> [b]
rights'' = snd . partitionEithers''

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' eithers = (lefts' eithers, rights' eithers)

partitionEithers'' :: [Either a b] -> ([a], [b])
partitionEithers'' = foldr (\e -> \(ls, rs) -> case e of
                               Left l -> (l : ls, rs)
                               Right r -> (ls, r : rs)) ([], [])

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' f e = either' (const Nothing) (Just . f) e
