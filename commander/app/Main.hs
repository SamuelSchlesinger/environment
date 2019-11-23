module Main where

import Commander 
import Prelude

main :: IO ()
main = command_ (toplevel @"commander-example" file)

file = sub @"writer" (arg @"filename" \filename -> 
                      arg @"contents" \contents -> 
                        raw $ writeFile filename contents)
   :+: sub @"reader" (arg @"filename" \filename ->                              
                        raw $ readFile filename >>= putStrLn)
