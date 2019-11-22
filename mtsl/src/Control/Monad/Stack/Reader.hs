module Control.Monad.Stack.Reader where

import Control.Monad.Stack.Trans
import Control.Monad.Stack.TFunctor

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor f => Functor (ReaderT r f) where
  fmap f = ReaderT . fmap (fmap f) . runReaderT

instance Applicative f => Applicative (ReaderT r f) where
  mf <*> ma = ReaderT \r -> runReaderT mf r <*> runReaderT ma r
  pure = ReaderT . const . pure

instance Monad m => Monad (ReaderT r m) where
  ma >>= f = ReaderT \r -> do
    a <- runReaderT ma r
    runReaderT (f a) r

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

class Monad m => MonadReader r m where
  {-# MINIMAL (ask | reader), local #-}
  ask :: m r
  local :: (r -> r) -> m a -> m a
  reader :: (r -> a) -> m a
  ask = reader id
  reader = flip fmap ask 

instance Monad m => MonadReader r (ReaderT r m) where
  ask = ReaderT return
  local f ma = ReaderT $ \r -> runReaderT ma (f r)
  reader s = ReaderT $ \r -> return (s r)

class (forall m. Monad m => MonadReader r (t m), MonadTrans t) => MonadReaderT r t
instance MonadReaderT r (ReaderT r)

instance TFunctor (ReaderT r) where
  tmap n = ReaderT . fmap n . runReaderT
