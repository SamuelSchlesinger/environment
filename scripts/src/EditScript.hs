{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
import System.Process
import System.IO
import System.Directory

-- trying to port my old way of doing things over.. shouldn't take too long

main :: IO ()
main = pure () {- getArgs >>= \case
  [scriptName] -> do
    env <- getEnv "ENVIRONMENT_BASE_FOLDER"
    let scriptSrcFilePath = env ++ scriptName ++ ".hs"
    let scriptExeFilePath = env ++ scriptName
    doesFileExist scriptSrcFilePath >>= \case
      True -> do 
        callProcess "chmod" ["+x", scriptExeFilePath]
        callProcess "vim" [scriptSrcFilePath]
        callProcess (env <> "build-hs-scripts") []
      False -> do
        hPutStr stderr ("new-hs-script: script with name " <> scriptName <> " does not already exist.")
  _ -> do
    hPutStr stderr "usage: new-hs-script <script-name>"
    -}
