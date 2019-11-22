{-# LANGUAGE TypeApplications #-}
module Control.Monad.Stack.Adjoint where

import Control.Monad
import Control.Monad.Stack.Trans
import Data.Functor.Identity
import Data.Functor.Compose

newtype AdjointT f g m a = AdjointT { runAdjointT :: f (m (g a)) } 

instance (Functor f, Functor g, Functor m) => Functor (AdjointT f g m) where
  fmap f = AdjointT . fmap (fmap (fmap f)) . runAdjointT  

instance (Adjunction f g, Monad m) => Applicative (AdjointT f g m) where
  pure = AdjointT . fmap return . unit
  (<*>) = ap

instance (Adjunction f g, Monad m) => Monad (AdjointT f g m) where
  m >>= f = AdjointT $ fmap (>>= counit . fmap (runAdjointT . f)) (runAdjointT m)

instance (Adjunction f g, Traversable g) => MonadTrans (AdjointT f g) where
  lift = AdjointT . fmap sequence . unit @f @g

-- The laws that this must satisfy are
-- fmap counit . unit = id
-- counit . fmap unit = id
-- or...
-- leftAdj counit = id
-- rightAdj unit  = it
class (Functor f, Functor g) => Adjunction f g where
  unit :: a -> f (g a)
  counit :: g (f a) -> a
  leftAdj :: (g a -> b) -> a -> f b
  rightAdj :: (a -> f b) -> g a -> b
  leftAdj f = fmap f . unit
  rightAdj f = counit . fmap f

-- Proof of the laws: 
-- Here, we can prove the first law with the help of GHC.
-- :t fmap (uncurry (flip ($))) . flip (,)
--   fmap (uncurry (flip ($))) . flip (,) :: forall a b. (a -> b) -> a -> b
-- The only function with this type is the identity
--
-- Lets try it for the second law.
-- :t uncurry (flip ($)) . fmap (flip (,))
--   uncurry (flip ($)) . fmap (flip (,)) :: (a, b) -> (a, b)
-- The same as above!
instance Adjunction ((->) b) ((,) b) where
  unit = flip (,)
  counit = uncurry (flip ($))

-- Proof of the laws can be seen through GHCi yet again, left as an
-- exercise to the reader.
instance Adjunction Identity Identity where
  unit = Identity . Identity
  counit = runIdentity . runIdentity

-- TODO Here, we have to do a little more work...
instance (Adjunction f g, Adjunction f' g') => Adjunction (Compose f' f) (Compose g g') where
  unit = Compose . leftAdj (leftAdj Compose)
  counit = rightAdj (rightAdj getCompose) . getCompose

-- TODO Implement the adjunction between Free and CoFree
