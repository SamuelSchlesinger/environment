module Main where

import Commander 
import Prelude
import Control.Monad
import Control.Exception

type FileReader = Arg "file" FilePath & Raw
type FileWriter = Arg "file" FilePath & Arg "string" String & Raw
type FileProgram = Flag "verbose" & ("read" & FileReader + "write" & FileWriter)

program :: ProgramT FileProgram IO ()
program = flag \verbosity -> sub (fileReader verbosity) <+> sub (fileWriter verbosity) where
  fileReader verbosity = arg \file -> raw $ do
    catch (readFile file >>= putStrLn) \(e :: SomeException) -> do
      when verbosity (putStrLn (show e))
  fileWriter verbosity = arg \file -> arg $ \text -> raw $ do
    catch (writeFile file text) \(e :: SomeException) -> do
      when verbosity (putStrLn (show e))

main :: IO ()
main = commander_ $ toplevel @"commander-example" program
