module Control.Monad.Stack.CoFree where

data CoFree f a = CoFree { coFreeHead :: a, coFreeTail :: f (CoFree f a) }
