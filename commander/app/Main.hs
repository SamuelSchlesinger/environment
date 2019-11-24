module Main where

import Commander 
import Prelude

main :: IO ()
main = command_ (toplevel @"commander-example" file)

file :: ProgramT ("writer" & Arg "filename" FilePath & Arg "contents" String & Raw
                + "reader" & Arg "filename" FilePath                         & Raw) IO ()
file = sub @"writer" (arg @"filename" \filename -> 
                      arg @"contents" \contents -> 
                        raw $ writeFile filename contents)
   :+: sub @"reader" (arg @"filename" \filename ->                              
                        raw $ readFile filename >>= putStrLn)
