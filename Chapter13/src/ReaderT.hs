module ReaderT where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import MonadTrans

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance (Monad m) => Functor (ReaderT r m) where
  fmap :: (Monad m) => (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap fn readerT =
    ReaderT $ \r -> fmap fn (runReaderT readerT r)

instance (Monad m) => Applicative (ReaderT r m) where
  pure :: (Monad m) => a -> ReaderT r m a
  pure a = ReaderT $ \_ -> pure a

  (<*>) :: (Monad m) => ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  apFn <*> readerT = ReaderT $ \r ->
    runReaderT apFn r <*> runReaderT readerT r

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  readerT >>= f = ReaderT $ \r -> do
    a <- runReaderT readerT r
    runReaderT (f a) r

instance MonadTrans (ReaderT r) where
  lift ma = ReaderT $ const ma

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

ask :: (Monad m) => ReaderT r m r
ask = ReaderT return

local :: (Monad m) => (r -> r) -> ReaderT r m a -> ReaderT r m a
local f m = ReaderT $ runReaderT m . f

instance (Monad m) => MonadReader r (ReaderT r m) where
  ask = ReaderT.ask
  local = ReaderT.local
