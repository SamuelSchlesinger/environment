module Main where

import Commander (commander, arg, raw, (<+>), sub)
import Prelude
import Control.Monad

main :: IO ()
main = do
  let fileReader = arg @"file" @FilePath (raw . (readFile >=> putStrLn))
  let fileWriter = arg @"file" @FilePath \file -> arg @"string" @String (raw . writeFile file) 
  let b = sub @"read" fileReader <+> sub @"write" fileWriter
  x <- commander b
  print x
