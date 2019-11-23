module Main where

import Commander 
import Prelude
import Control.Monad
import Control.Exception
import Control.Applicative (empty)

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

main'' :: IO ()
main'' = commander_ . toplevel @"file-program" $ raw $ putStrLn "hello world"

main :: IO ()
main = commander_ . toplevel @"file-program" $ (sub @"file-reader" fileReader <+> sub @"file-writer" fileWriter) where
  fileWriter = 
    arg @"file-name" \fileName -> 
    arg @"new-contents" \newContents -> 
    flag @"print-former-contents" \printFormerContents -> raw do
      when printFormerContents $ do
        formerContents <- readFile fileName 
        putStrLn formerContents
      writeFile fileName newContents
  fileReader = 
    arg @"file-name" \fileName -> 
    opt @"print-after" @"print-after" @"p" \printAfter -> raw do
      contents <- readFile fileName 
      putStrLn contents 
      maybe empty putStrLn printAfter

main' :: IO ()
main' = commander_ $ toplevel @"commander-example" program
