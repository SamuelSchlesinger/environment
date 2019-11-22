{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Category.Adjunction where

import Control.Category.Functor
import Data.Functor.Adjunction
import Control.Category

class (Category c, Category d, Functor' c d f, Functor' d c g) => Adjunction' c d f g | c d f -> g, c d g -> f where
  unit' :: c a (g (f a))
  counit' :: d (f (g a)) a  
  rightAdjunct' :: c a (g b) -> d (f a) b
  leftAdjunct' :: d (f a) b -> c a (g b)

instance Adjunction f g => Adjunction' (->) (->) f g where
  unit' = unit 
  counit' = counit
  leftAdjunct' = leftAdjunct
  rightAdjunct' = rightAdjunct
