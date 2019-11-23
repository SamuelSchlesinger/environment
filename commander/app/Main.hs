module Main where

import Commander 
import Prelude
import Data.Text (unpack)
import Control.Monad

type FileReader = Arg "file" FilePath ... Raw
type FileWriter = Arg "file" FilePath ... Arg "string" String ... Raw
type Help = Flag "nohelp" ... Raw
type FileProgram = Flag "verbose" ... ("read" ... FileReader + "write" ... FileWriter + Help)

program :: ProgramT FileProgram IO
program = flag \verbosity -> sub (fileReader verbosity) <+> sub (fileWriter verbosity) <+> help where
  fileReader verbose = arg \file -> raw $ do
    readFile file >>= putStrLn 
    if verbose then putStrLn "Finished"
               else return ()
  fileWriter verbose = arg \file -> arg $ \text -> raw $ do
    writeFile file text
    if verbose then putStrLn "Finished" 
               else return () 
  help = flag \helpOff -> 
    if not helpOff then raw $ do
      putStrLn "usage: "
      void $ traverse (putStrLn . unpack) $ invocations @FileProgram 
    else raw $ pure ()

main :: IO ()
main = commander_ $ toplevel @"commander-example" program
