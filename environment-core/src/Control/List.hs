module Control.List where

import Control.Natural
import GHC.Exts (Constraint)

type family Index (t :: k) (ts :: [k]) where
  Index t (t ': ts) = 'Z
  Index t (x ': ts) = 'S (Index t ts)

type Prior t1 t2 ts = Compare (Index t1 ts) (Index t2 ts) 'LT

type family Ordered (ms :: [k]) (ts :: [k]) :: Constraint where
  Ordered (x ': y ': ms) ts = (Prior x y ts, Ordered (y ': ms) ts)
  Ordered (x ': '[])     ts = ()
  Ordered '[]            ts = ()

type family ForAll (cs :: [k]) (prop :: k -> Constraint) :: Constraint where
  ForAll (c ': cs) prop = (prop c, ForAll cs prop)
  ForAll '[] prop = ()
