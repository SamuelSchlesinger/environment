{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Control.Monad
import Control.Monad.Stack
import Control.Monad.Stack.State
import Control.Monad.Stack.Error
import Control.Monad.Stack.List
import Control.Monad.Stack.Writer
import Control.Monad.Stack.Reader
import Control.Monad.Stack.Trans

example0 :: ( MonadStateT  Int     (Stack ts)
           , MonadErrorT  String  (Stack ts)
           , MonadStateT  Int     st
           , MonadErrorT  String  et
           , Ordered '[st, et] ts
            ) => Stack ts IO ()
example0 = do
  lift $ putStrLn "Hello, world"
  x <- do
    lift $ putStrLn "Hello, again"
    put @Int 5
    throw @String "Hello"
  lift $ putStrLn $ show "yes"

example0' :: Stack '[StateT Int, ErrorT String] IO ()
example0' = example0 @_ @(StateT Int) @(ErrorT String)

main0 :: IO ()
main0 = do
  x <-   runStack
       . popErrorT
       . popStateT 3 
       $ example0'
  print x

example1 :: MonadReaderT Bool (Stack ts) => Stack ts IO ()
example1 = do
  lift $ putStrLn "Running example 1"
  x <- ask @Bool
  lift $ print x

example1' :: Stack '[ErrorT Bool, StateT Int, WriterT [Bool], ReaderT Bool] IO ()
example1' = example1 

main1 :: IO ()
main1 = do
  x <- runStack
     . popReaderT True
     . popWriterT
     . popStateT 40
     . popErrorT
     $ example1'
  print x

example2 :: Stack '[StateT Int] IO ()
example2 = do
  x <- get @Int
  put @Int (x + 10)
  x' <- get @Int
  lift $ print x'

main2 :: IO ()
main2 = void $ runStack . popStateT 10 $ example2

main :: IO ()
main = main0
