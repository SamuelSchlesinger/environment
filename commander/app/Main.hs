module Main where

import Commander 
import Prelude
import Control.Monad
import Control.Applicative (empty)

main :: IO ()
main = command_ . toplevel @"file-program" $ (sub @"file-reader" fileReader <+> sub @"file-writer" fileWriter) where
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
