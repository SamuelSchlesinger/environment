module Control.Monad.Stack.Writer where

import Control.Monad.Stack.TFunctor
import Control.Monad.Stack.Trans

newtype WriterT w m a = WriterT { runWriterT :: m (w, a) }

instance (Monoid w, Functor f) => Functor (WriterT w f) where
  fmap f = WriterT . fmap (\(w, a) -> (w, f a)) . runWriterT

instance (Monoid w, Applicative f) => Applicative (WriterT w f) where
  pure = WriterT . pure . ((,) mempty)
  mf <*> ma = WriterT $ (fmap (\(w0, f) -> (\(w1, x) -> (w0 <> w1, f x))) (runWriterT mf)) <*> runWriterT ma

instance (Monoid w, Monad f) => Monad (WriterT w f) where
  ma >>= f = WriterT $ do
    (w0, a) <- runWriterT ma
    (w1, b) <- runWriterT (f a)
    return (w0 <> w1, b)

instance Monoid w => MonadTrans (WriterT w) where
  lift = WriterT . fmap \x -> (mempty, x)

class (Monoid w, Monad m) => MonadWriter w m where
  {-# MINIMAL (writer | tell), listen, pass #-}
  writer :: (w, a) -> m a
  tell :: w -> m ()
  listen :: m a -> m (w, a)
  pass :: m (w -> w, a) -> m a
  tell w = writer (w, ()) 
  writer (w, a) = tell w >> return a

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  writer = WriterT . return
  listen ma = WriterT $ do
    (w, a) <- runWriterT ma
    return (w, (w, a)) 
  pass m = WriterT $ do
    (w, (f, a)) <- runWriterT m
    return (f w, a)
  tell w = WriterT $ return (w, ())

class (Monoid w, forall m. Monad m => MonadWriter w (t m), MonadTrans t) => MonadWriterT w t
instance Monoid w => MonadWriterT w (WriterT w)

instance Monoid w => TFunctor (WriterT w) where
  tmap t = WriterT . t . runWriterT
