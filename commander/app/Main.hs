module Main where

import Commander 
import Prelude
{-import Control.Monad
import Control.Applicative (empty)-}

main :: IO ()
main = command_ file
{-
type File = "writer" & Arg "file" FilePath & Arg "contents" FilePath Raw
          + "reader" & Arg "file" FilePath & Raw

file :: ProgramT File IO
file = sub (arg \file -> arg \contents -> raw $ writeFile file contents)
   :+: sub (arg \file -> raw $ readFile file >>= putStrLn)
-}
file = sub @"writer" (arg @"filename" \filename -> arg @"contents" \contents -> raw $ writeFile filename contents)
   :+: sub @"reader" (arg @"filename" \filename ->                              raw $ readFile filename >>= putStrLn)

{-main :: IO ()
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
      -}
