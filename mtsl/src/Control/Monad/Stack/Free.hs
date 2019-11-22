{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Control.Monad.Stack.Free where

import Control.Monad
import Control.Monad.Stack.Trans

data FreeF f a b = Free (f b) | Pure a

newtype FreeT f m a 
  = FreeT { runFreeT :: m (FreeF f a (FreeT f m a)) }

instance (Functor f, Monad m) => Functor (FreeT f m) where
  fmap f (FreeT m) = FreeT (liftM f' m) where
    f' (Pure a) = Pure (f a)
    f' (Free as)  = Free (fmap (fmap f) as)

instance (Functor f, Monad m) => Applicative (FreeT f m) where
  (<*>) = ap
  pure = FreeT . return . Pure

instance (Functor f, Monad m) => Monad (FreeT f m) where
  x >>= f = FreeT $ runFreeT x >>= \case
    Pure a -> runFreeT (f a)
    Free w -> return (Free (fmap (>>= f) w))

instance Functor f => MonadTrans (FreeT f) where
  lift = FreeT . fmap Pure
