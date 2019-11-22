module Main where

import Commander (commander, arg, raw, (<+>), sub, flag)
import Prelude
import Control.Monad

main :: IO ()
main = do
  let fileReader verbose = arg @"file" @FilePath (raw . (readFile >=> putStrLn >=> if verbose then \_ -> putStrLn "Finished!" else return))
  let fileWriter verbose = arg @"file" @FilePath \file -> arg @"string" @String (raw . (writeFile file >=> if verbose then \_ -> putStrLn "Finished" else return)) 
  let b = flag @"verbose" \v -> sub @"read" (fileReader v) <+> sub @"write" (fileWriter v)
  x <- commander b
  print x
