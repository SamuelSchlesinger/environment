module Control.Number where

data N = S N | Z

class Add (a :: N) (b :: N) (c :: N)

instance Add 'Z b b
instance Add a b c => Add ('S a) b ('S c)


