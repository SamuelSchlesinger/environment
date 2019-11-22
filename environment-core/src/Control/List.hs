module Control.List where

import Control.Natural
import GHC.Types (Constraint)

type family Index (t :: k) (ts :: [k]) where
  Index t (t ': ts) = 'Z
  Index t (x ': ts) = 'S (Index t ts)

type Prior t1 t2 ts = Compare (Index t1 ts) (Index t2 ts) 'LT

type family Ordered (ms :: [k]) (ts :: [k]) :: Constraint where
  Ordered (x ': y ': ms) ts = (Prior x y ts, Ordered (y ': ms) ts)
  Ordered (x ': '[])     ts = ()
  Ordered '[]            ts = ()