module OuterInner where
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- We only need to use return once -- because it's one big Monad

embedded :: MaybeT
            (ExceptT String
              (ReaderT () IO))
            Int
embedded = return 1

maybeUnwrap :: ExceptT
               String
               (ReaderT () IO)
               (Maybe Int)
maybeUnwrap = runMaybeT embedded

-- Next
eitherUnwrap :: ReaderT ()
                IO
                (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

-- Lastly
readerUnwrap :: () -> IO (Either
                          String
                          (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

readerWrap :: ReaderT ()
              IO
              (Either String (Maybe Int))
readerWrap = ReaderT readerUnwrap

eitherWrap :: ExceptT
              String
              (ReaderT () IO)
              (Maybe Int)
eitherWrap = ExceptT readerWrap

maybeWrap :: MaybeT
             (ExceptT String
               (ReaderT () IO))
             Int
maybeWrap = MaybeT eitherWrap

embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = maybeWrap


embedded'' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded'' = MaybeT . ExceptT . ReaderT . (fmap return) $ (const (Right (Just 1)))
