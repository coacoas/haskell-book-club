{-# LANGUAGE InstanceSigs #-}

module EitherT where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype EitherT e m a =  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT mfab) <*> (EitherT mea) = EitherT $ (<*>) <$> mfab <*> mea

instance Monad m => Monad (EitherT e m) where
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT ema) >>= f = EitherT $ do
    ma <- ema
    case ma of
      Left e -> return $ Left e
      Right a -> runEitherT $ f a

instance MonadTrans (EitherT e) where
  lift :: (Monad m) => m a -> EitherT e m a
  lift = EitherT . liftM Right


swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) =
  EitherT $ swapEither <$> ema

swapEitherT' :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT' = EitherT . (swapEither <$>) . runEitherT

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT famc fbmc (EitherT amb) =
  amb >>= (either famc fbmc)
