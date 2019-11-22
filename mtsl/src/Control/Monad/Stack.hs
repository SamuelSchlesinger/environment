module Control.Monad.Stack where

import Control.Applicative
import Control.Monad
import Control.Monad.Stack.TFunctor
import Control.Monad.Stack.Trans
import Control.Monad.Stack.Constraint
import Control.Monad.Stack.Writer
import Control.Monad.Stack.Error
import Control.Monad.Stack.Reader
import Control.Monad.Stack.State
import Data.Kind


-- | The kind signature of a transformer, for convenience sake.
type Transformer = (Type -> Type) -> Type -> Type

-- | The direct composition of a type level list of transformers.
type family Stack'' (ts :: [Transformer]) (m :: Type -> Type) :: Type -> Type where
  Stack'' '[] m = m
  Stack'' (t ': ts) m = t (Stack'' ts m)

-- | The interleaved composition of a type level list of transformers
-- with the 'Stack' newtype.
type family Stack' (ts :: [Transformer]) (m :: Type -> Type) :: Type -> Type where
  Stack' '[] m = m
  Stack' (t ': ts) m = t (Stack ts m)

-- | A newtype around @Stack'@.
newtype Stack (ts :: [Transformer]) (m :: Type -> Type) (a :: Type) 
  = Stack { runStack :: Stack' ts m a }

instance Functor f => Functor (Stack '[] f) where
  fmap f = Stack . fmap f . runStack

instance Applicative f => Applicative (Stack '[] f) where
  ff <*> fa = Stack $ runStack ff <*> runStack fa
  pure = Stack . pure

instance Monad f => Monad (Stack '[] f) where
  ma >>= f = Stack $ runStack ma >>= (runStack . f)

instance Alternative f => Alternative (Stack '[] f) where
  Stack a <|> Stack b = Stack (a <|> b)
  empty = Stack empty

instance MonadPlus f => MonadPlus (Stack '[] f)

instance (Monad (Stack ts m), MonadTrans t) => Functor (Stack (t ': ts) m) where
  fmap f = Stack . fmap f . runStack 

instance (Monad (Stack ts m), MonadTrans t) => Applicative (Stack (t ': ts) m) where
  (<*>) = ap
  pure  = Stack . pure

instance (Monad (Stack ts m), MonadTrans t) => Monad (Stack (t ': ts) m) where
  ma >>= f = Stack $ do
    a <- runStack ma
    runStack $ f a

instance MonadTrans (Stack '[]) where
  lift = Stack

instance (MonadTrans t, MonadTrans (Stack ts)) => MonadTrans (Stack (t ': ts)) where
  lift = Stack . lift . lift

instance TFunctor (Stack '[]) where
  tmap t = Stack . t . runStack

instance (TFunctor t, TFunctor (Stack ts)) => TFunctor (Stack (t ': ts)) where
  tmap t = Stack . tmap (tmap t) . runStack

liftStack :: (Monad (Stack ts m), MonadTrans t1) => Stack ts m a -> Stack (t1 : ts) m a
liftStack = Stack . lift

liftStack2 :: ( ForAll Monad '[Stack ts m, Stack (t2 : ts) m]
              , ForAll MonadTrans '[t1, t2]) => Stack ts m a -> Stack (t1 : t2 : ts) m a
liftStack2 = liftStack . liftStack

liftStack3 :: ( ForAll Monad '[Stack ts m
                             , Stack (t3 : ts) m
                             , Stack (t2 : t3 : ts) m]
              , ForAll MonadTrans '[t1, t2, t3]) => Stack ts m a -> Stack (t1 : t2 : t3 : ts) m a
liftStack3 = liftStack2 . liftStack

liftStack4 :: ( ForAll Monad '[Stack ts m
                             , Stack (t4 : ts) m
                             , Stack (t3 : t4 : ts) m
                             , Stack (t2 : t3 : t4 : ts) m]
              , ForAll MonadTrans '[t1, t2, t3, t4]) => Stack ts m a -> Stack (t1 : t2 : t3 : t4 : ts) m a
liftStack4 = liftStack2 . liftStack2

liftStack5 :: ( ForAll Monad '[Stack (t5 : ts) m
                             , Stack (t4 : t5 : ts) m
                             , Stack (t3 : t4 : t4 : ts) m
                             , Stack (t2 : t3 : t4 : t5 : ts) m
                             , Stack ts m]
              , ForAll MonadTrans '[t1, t2, t3, t4, t5]) => Stack ts m a -> Stack (t1 : t2 : t3 : t4 : t5 : ts) m a
liftStack5 = liftStack3 . liftStack2

instance ( Monad m
         , Monoid w
         , MonadTrans (Stack ts) ) => MonadWriter w (Stack (WriterT w ': ts) m) where
  tell w = Stack . WriterT $ do
    return (w, ())
  listen ma = Stack . WriterT $ do
    (w, a) <- runWriterT . runStack $ ma
    return (w, (w, a))
  pass m = Stack . WriterT $ do
    (w, (f, a)) <- runWriterT . runStack $ m
    return (f w, a)
  writer (w, a) = Stack . WriterT $ return (w, a)

instance ( MonadWriter w m
         , Monoid w ) => MonadWriter w (Stack '[] m) where
  tell = Stack . tell
  listen = Stack . listen . runStack
  pass = Stack . pass . runStack
  writer = Stack . writer

instance ( Monad m
         , Monoid w
         , MonadWriterT w (Stack ts)) => MonadWriter w (Stack (ReaderT r ': ts) m) where
  tell = Stack . lift . tell
  listen ma = Stack . ReaderT $ \r -> do
    listen $ runReaderT (runStack ma) r
  pass m = Stack . ReaderT $ \r -> do
    pass $ runReaderT (runStack m) r

instance ( Monad m
         , Monoid w
         , MonadTrans (Stack ts)
         , MonadWriterT w (Stack ts)) => MonadWriter w (Stack (StateT s ': ts) m) where
  tell = Stack . lift . tell
  listen ma = Stack . StateT $ \s -> do
    (w, (s', a)) <- listen $ runStateT (runStack ma) s
    return (s', (w, a))
  pass m = Stack . StateT $ \s -> pass $ do
    (s', (f, a)) <- runStateT (runStack m) s
    return (f, (s', a))

instance ( Monad m
         , Monoid w
         , MonadWriterT w (Stack ts)) => MonadWriter w (Stack (ErrorT e ': ts) m) where
  tell = Stack . lift . tell
  listen ma = Stack . ErrorT $ do
    (w, ea) <- listen $ runErrorT (runStack ma)
    case ea of
      Left e -> return (Left e)
      Right a -> return $ Right (w, a)
  pass m = Stack . ErrorT $ pass $ do
    ea <- runErrorT (runStack m)
    case ea of
      Left e -> return (id, Left e)
      Right s -> return (fmap Right s)

--instance ( Monad m
--         , Monoid w
--         , Monad f
--         , Traversable f
--         , Traversable g
--         , Monad g
--         , Adjunction f g
--         , MonadWriterT w (Stack ts)) => MonadWriter w (Stack (AdjointT f g ': ts) m) where
--  tell a = liftStack (tell a)
--  pass m = Stack . AdjointT $ return $ pass @w @(Stack ts m) $ do
--    x <- sequence $ runAdjointT (runStack m)
--    return $ fmap pure $ counit $ sequence x

instance (MonadTrans (Stack ts), Monoid w) => MonadWriterT w (Stack (WriterT w ': ts))
instance MonadWriterT w (Stack ts) => MonadWriterT w (Stack (ReaderT r ': ts))
instance MonadWriterT w (Stack ts) => MonadWriterT w (Stack (StateT r ': ts))
instance MonadWriterT w (Stack ts) => MonadWriterT w (Stack (ErrorT e ': ts))

popWriterT :: Stack (WriterT w ': ts) m a -> Stack ts m (w, a)
popWriterT = runWriterT . runStack

instance ( Monad m
         , MonadTrans (Stack ts) ) => MonadReader r (Stack (ReaderT r ': ts) m) where
  ask = Stack ask
  local f ma = Stack . ReaderT $ \r -> runReaderT (runStack ma) (f r)
  reader f = Stack (reader f)

instance MonadReader r m => MonadReader r (Stack '[] m) where
  ask = Stack ask
  local f ma = Stack $ local f $ runStack ma
  reader = Stack . reader

instance ( Monad m
         , Monoid w
         , MonadReaderT r (Stack ts) ) => MonadReader r (Stack (WriterT w ': ts) m) where
  ask = Stack . WriterT $ do
    x <- ask
    return (mempty, x)
  local f ma = Stack . WriterT $ local f $ runWriterT (runStack ma)

instance ( Monad m
         , MonadReaderT r (Stack ts) ) => MonadReader r (Stack (StateT s ': ts) m) where
  ask = Stack . StateT $ \s -> do
    x <- ask
    return (s, x)
  local f ma = Stack . StateT $ local f . runStateT (runStack ma)

instance ( Monad m
         , MonadReaderT r (Stack ts)) => MonadReader r (Stack (ErrorT s ': ts) m) where
  ask = Stack . ErrorT $ fmap Right ask
  local f ma = Stack . ErrorT $ local f $ runErrorT (runStack ma)

instance MonadTrans (Stack ts) => MonadReaderT r (Stack (ReaderT r ': ts))
instance (Monoid w, MonadReaderT r (Stack ts)) => MonadReaderT r (Stack (WriterT w ': ts))
instance MonadReaderT r (Stack ts) => MonadReaderT r (Stack (StateT s ': ts)) 
instance MonadReaderT r (Stack ts) => MonadReaderT r (Stack (ErrorT e ': ts))

popReaderT :: r -> Stack (ReaderT r ': ts) m a-> Stack ts m a
popReaderT = flip (runReaderT . runStack)

instance MonadState s m => MonadState s (Stack '[] m) where
  state s = Stack (state s)

instance ( Monad m
         , MonadTrans (Stack ts) ) => MonadState s (Stack (StateT s ': ts) m) where
  state s = Stack (state s)
  
instance ( Monad m
         , MonadState s (Stack ts m) ) => MonadState s (Stack (ReaderT r ': ts) m) where
  state s = Stack . ReaderT . const $ state s

instance ( Monad m
         , Monoid w
         , MonadState s (Stack ts m) ) => MonadState s (Stack (WriterT w ': ts) m) where
  state s = Stack . WriterT $ do
    a <- state s
    return (mempty, a)

instance ( Monad m
         , MonadState s (Stack ts m) ) => MonadState s (Stack (ErrorT w ': ts) m) where
  state s = Stack . ErrorT $ fmap Right $ state s

instance MonadTrans (Stack ts) => MonadStateT s (Stack (StateT s ': ts))
instance (Monoid w, MonadStateT s (Stack ts)) => MonadStateT s (Stack (WriterT w ': ts))
instance MonadStateT s (Stack ts) => MonadStateT s (Stack (ReaderT r ': ts))
instance MonadStateT s (Stack ts) => MonadStateT s (Stack (ErrorT e ': ts)) 

popStateT :: s -> Stack (StateT s ': ts) m a -> Stack ts m (s, a)
popStateT = flip $ runStateT . runStack

instance MonadError e m => MonadError e (Stack '[] m) where
  throw = Stack . throw
  catch ma h = Stack (catch (runStack ma) (runStack . h))

instance ( Monad m
         , MonadTrans (Stack ts) ) => MonadError e (Stack (ErrorT e ': ts) m) where
  throw = Stack . throw
  catch ma h = Stack . ErrorT $ do
    ea <- runErrorT . runStack $ ma
    case ea of
      Left e -> runErrorT . runStack $ h e
      Right a -> return (Right a)

instance ( Monad m
         , MonadError e (Stack ts m) ) => MonadError e (Stack (StateT s ': ts) m) where
  throw = liftStack . throw 
  catch ma h = Stack . StateT $ \s -> catch (runStateT (runStack ma) s) (flip (runStateT . runStack . h) s) 

instance ( Monad m
         , MonadError e (Stack ts m) ) => MonadError e (Stack (ReaderT s ': ts) m) where
  throw = liftStack . throw
  catch ma h = Stack . ReaderT $ \s -> catch (runReaderT (runStack ma) s) (flip (runReaderT . runStack . h) s)

instance ( Monad m
         , Monoid w
         , MonadError e (Stack ts m) ) => MonadError e (Stack (WriterT w ': ts) m) where
  throw = liftStack . throw
  catch ma h = Stack . WriterT $ catch (runWriterT (runStack ma)) (runWriterT . runStack . h)

instance MonadTrans (Stack ts) => MonadErrorT e (Stack (ErrorT e ': ts))
instance (Monoid w, MonadErrorT e (Stack ts)) => MonadErrorT e (Stack (WriterT w ': ts))
instance MonadErrorT e (Stack ts) => MonadErrorT e (Stack (ReaderT r ': ts))
instance MonadErrorT e (Stack ts) => MonadErrorT e (Stack (StateT s ': ts))

popErrorT :: Stack (ErrorT e ': ts) m a -> Stack ts m (Either e a)
popErrorT = runErrorT . runStack
