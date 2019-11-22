module Control.Monad.Stack.Error where

import Control.Monad
import Control.Monad.Stack.Trans

newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }

instance Functor f => Functor (ErrorT e f) where
  fmap f = ErrorT . fmap (fmap f) . runErrorT 

instance Monad m => Applicative (ErrorT e m) where
  (<*>) = ap
  pure  = return

instance Monad m => Monad (ErrorT e m) where
  ma >>= f = ErrorT $ do
    ea <- runErrorT ma
    case ea of
      Left e -> return (Left e)
      Right a -> runErrorT $ f a
  return = ErrorT . return . Right

instance MonadTrans (ErrorT e) where
  lift = ErrorT . fmap Right

class Monad m => MonadError e m where
  throw :: e -> m a
  catch :: m a -> (e -> m a) -> m a

instance Monad m => MonadError e (ErrorT e m) where
  throw = ErrorT . return . Left 
  catch ma h = ErrorT $ do
    ea <- runErrorT ma
    case ea of
      Left e -> runErrorT $ h e
      _ -> return ea

class (forall m. Monad m => MonadError e (t m)) => MonadErrorT e t
instance MonadErrorT e (ErrorT e) 
