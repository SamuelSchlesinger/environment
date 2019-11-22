module Control.Monad.Stack.TFunctor where

import Control.Monad.Stack.Trans

class MonadTrans t => TFunctor t where
  tmap :: (forall x. m x -> n x) -> t m a -> t n a
