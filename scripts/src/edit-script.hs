{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
import System.Process
import System.IO
import System.Directory
import Commander
import Data.Text (unpack)
import Control.Monad

type EditScript = Named "edit-script" ... Arg "script-name" String ... Raw + Raw

usage :: ProgramT Raw IO
usage = raw $ do
  putStrLn "usage: "
  void $ traverse (putStrLn . unpack) (invocations @EditScript)

main :: IO ()
main = commander_ . (<+> usage) $ arg @"script-name" @FilePath \scriptName -> raw $ do
  env <- getEnv "ENVIRONMENT_BASE_PATH"
  let scriptFilePath = env ++ "/scripts/src/" ++ scriptName ++ ".hs"
  doesFileExist scriptFilePath >>= \case
    True -> do 
      setCurrentDirectory env
      callProcess "vim" [scriptFilePath]
      callProcess "stack" ["install"]
    False -> do
      hPutStr stderr ("edit-script: script with name " <> scriptName <> " does not already exist.")
