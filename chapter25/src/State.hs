module State where

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT smas) = StateT $ (fmap . fmap) f (runStateT s)
