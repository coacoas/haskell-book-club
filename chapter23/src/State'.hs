{-# LANGUAGE InstanceSigs #-}
module State' where

newtype State' s a = State' { runState' :: s -> (a,s) }

instance Functor (State' s) where
  fmap :: (a -> b) -> State' s a -> State' s b
  fmap f (State' g) = State' $ \s ->
    let (a, next) = g s
        b = f a
    in (b, next)


instance Applicative (State' s) where
  pure :: a -> State' s a
  pure a = State' $ \s -> (a, s)

  (<*>) :: State' s (a->b) -> State' s a -> State' s b
  (State' f) <*> (State' g) = State' $ \s -> 
    let (ab, s1) = f s
        (ga, s2) = g s1
    in (ab ga, s2)

instance Monad (State' s) where
  return = pure
  
  (>>=) :: State' s a -> (a -> State' s b) -> State' s b
  (State' f) >>= g = State' $ \s ->
    let (fa, s1) = f s
        (gb, s2) = runState' (g fa) $ s1
    in (gb, s2)
