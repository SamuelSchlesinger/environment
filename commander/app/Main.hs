module Main where

import Commander (commander, arg, raw, (<+>), sub)

main :: IO ()
main = do
  let l = arg @"name" @String (raw . putStrLn . ("Hello, " <>))
  let r = arg @"town" @String (raw . putStrLn . ("I have never been to " <>))
  let b = sub @"hello" l <+> sub @"where" r
  x <- commander b
  print x
