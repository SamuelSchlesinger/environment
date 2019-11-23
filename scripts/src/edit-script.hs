{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
import System.Process
import System.IO
import System.Directory
import Commander

main :: IO ()
main = commander_ . toplevel @"edit-script" $ arg @"script-name" \scriptName -> raw $ do
  env <- getEnv "ENVIRONMENT_BASE_PATH"
  let scriptFilePath = env ++ "/scripts/src/" ++ scriptName ++ ".hs"
  doesFileExist scriptFilePath >>= \case
    True -> do 
      setCurrentDirectory env
      callProcess "vim" [scriptFilePath]
      callProcess "stack" ["install", "scripts:" ++ scriptName]
    False -> do
      hPutStr stderr ("edit-script: script with name " <> scriptName <> " does not already exist.")
