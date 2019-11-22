module Main where

import Commander (commander, arg, raw)

main :: IO ()
main = do
  x <- commander $ arg @"name" @String \name -> raw $ do
    putStrLn $ "My name is " <> name
  print x
