module Main where

import Commander 
import Prelude
import Data.Text (unpack)
import Control.Monad

type FileReader = Arg "file" FilePath ... Raw
type FileWriter = Arg "file" FilePath ... Arg "string" String ... Raw
type Help = Raw
type FileProgram = Named "commander-example" ... Flag "verbose" ... ("read" ... FileReader + "write" ... FileWriter + Help)

program :: ProgramT FileProgram IO
program = named $ flag \verbosity -> sub (fileReader verbosity) <+> sub (fileWriter verbosity) <+> help where
  fileReader verbose = arg(raw . (readFile >=> putStrLn >=> if verbose then \_ -> putStrLn "Finished!" else return))
  fileWriter verbose = arg \file -> arg (raw . (writeFile file >=> if verbose then \_ -> putStrLn "Finished" else return)) 
  help = raw . void $ traverse (putStrLn . unpack) (invocations @FileProgram)

main :: IO ()
main = commander_ program
