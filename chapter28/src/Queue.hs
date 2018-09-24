module Queue where

data Queue a = Queue { enqueue :: [a]
                     , dequeue :: [a]
                     } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push x (Queue en de) = Queue (x:en) de

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue e (d:ds)) = Just (d, Queue e ds)
pop (Queue (e:es) []) = Just (e, Queue [] (reverse es))
