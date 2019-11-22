module Control.Category.Functor where

import Control.Monad
import Control.Comonad
import Prelude hiding ((.), id)
import Control.Category

class (Category c, Category d) => Functor' c d f where
  fmap' :: c x y -> d (f x) (f y)
  
class Functor' c c f => Monad' c f where
  join' :: c (f (f y)) (f y)
  return' :: c y (f y) 
  bind' :: c x (f y) -> c (f x) (f y)
  join' = bind' id
  bind' f = join' . fmap' f

class Functor' c c f => Comonad' c f where
  extract' :: c (f y) y
  duplicate' :: c (f y) (f (f y))
  extend' :: c (f x) y -> c (f x) (f y)

instance Functor f => Functor' (->) (->) f where
  fmap' = fmap

instance Monad f => Monad' (->) f where
  join' = join
  return' = return
  bind' = flip (>>=)

instance Comonad f => Comonad' (->) f where
  extract' = extract
  duplicate' = duplicate
  extend' = extend
