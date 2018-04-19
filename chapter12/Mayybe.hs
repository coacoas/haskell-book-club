module Mayybe where

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust
-- isNothing Nothing  = True
-- isNothing (Just _) = False


mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes maybes = concat $ map maybeToList maybes

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe maybes = if (any isNothing maybes)
                   then Nothing
                   else Just (catMaybes maybes)
