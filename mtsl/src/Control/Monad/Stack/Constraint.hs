module Control.Monad.Stack.Constraint where

import GHC.Types (Constraint)

type family ForAll (prop :: k -> Constraint) (ks :: [k]) :: Constraint where
  ForAll prop '[] = ()
  ForAll prop (x ': xs) = (prop x, ForAll prop xs)
