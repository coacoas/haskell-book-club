{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
module State where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT smas) = StateT $ \s -> fmapfst f <$> smas s
    where fmapfst f (a, b) = (f a, b)
















instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smab) <*> (StateT sma) = StateT $ \s -> do
    (f, s1) <- smab s
    (a, s2) <- sma s1
    return (f a, s2)
























instance (Monad m) => Monad (StateT s m) where
  return = pure
  sma >>= f = StateT $ \s -> do
    (a, s') <- runStateT sma $ s
    runStateT (f a) s'


























instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)


instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO
