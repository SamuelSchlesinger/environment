{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Stack.N where

data N = Z | S N

class Compare (a :: N) (b :: N) (o :: Ordering) | a b -> o
instance Compare 'Z 'Z 'EQ
instance Compare ('S n) 'Z 'GT
instance Compare 'Z ('S n) 'LT
instance Compare n m o => Compare ('S n) ('S m) o
