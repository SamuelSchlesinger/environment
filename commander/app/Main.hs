module Main where

import Commander 
import Prelude

type File = Arg "filename" FilePath 
          & ("write" & Arg "contents" String & Raw
          +  "read"  & Raw) 

file :: ProgramT File IO ()
file = arg \a -> (sub $ arg (raw . writeFile a)) :+: (sub . raw $ readFile a >>= putStrLn)

main :: IO ()
main = command_ (toplevel @"commander-example" file)
