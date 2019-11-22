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
  let scriptSrcFilePath = env ++ "/scripts/src/" ++ scriptName ++ ".hs"
  doesFileExist scriptSrcFilePath >>= \case
    True -> do 
      setCurrentDirectory env
      callProcess "vim" [scriptSrcFilePath]
      callProcess "stack" ["install"]
    False -> do
      hPutStr stderr ("edit-hs-script: script with name " <> scriptName <> " does not already exist.")
