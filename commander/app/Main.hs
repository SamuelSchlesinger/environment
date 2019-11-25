module Main where

import Commander 
import Prelude

type File = Named "file"
          & Arg "filename" FilePath 
          & ("write" & Arg "contents" String & Raw
          +  "read"  & Raw) 

file :: ProgramT File IO ()
file = named $ arg \a -> (sub $ arg (raw . writeFile a)) :+: (sub . raw $ readFile a >>= putStrLn)

main :: IO ()
main = command_ (file :+: usage @File)
