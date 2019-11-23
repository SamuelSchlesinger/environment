{-# LANGUAGE UndecidableInstances #-}
module Control.Natural where

data N = S N | Z

class Add (a :: N) (b :: N) (c :: N)

instance Add 'Z b b
instance Add a b c => Add ('S a) b ('S c)

class Compare (a :: N) (b :: N) (o :: Ordering) | a b -> o
instance Compare 'Z 'Z 'EQ
instance Compare ('S n) 'Z 'GT
instance Compare 'Z ('S n) 'LT
instance Compare n m o => Compare ('S n) ('S m) o

