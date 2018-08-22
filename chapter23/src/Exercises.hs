module Exercises where

import State'

get :: State' s s
get = State' $ \s -> (s, s)

put :: s -> State' s ()
put s = State' $ \_ -> ((), s)

exec :: State' s a -> s -> s
exec (State' sa) = snd . sa

eval :: State' s a -> s -> a
eval (State' sa) = fst . sa

modify :: (s -> s) -> State' s ()
modify f = get >>= (put . f)

modify' :: (s -> s) -> State' s ()
modify' f = do
  s1 <- get
  put $ f s1
