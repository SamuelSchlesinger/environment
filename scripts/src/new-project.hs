import Commander
import System.Directory
import System.Process
import System.Environment

main :: IO ()
main = command_ . toplevel @"new-project" $ arg @"project-name" \projectName -> raw do
  currentDir <- getCurrentDirectory
  env <- getEnv "ENVIRONMENT_BASE_PATH"
  let projectDir = currentDir <> "/" <> projectName
  callProcess "cp" ["-R", env ++ "/project-template", projectDir]
  setCurrentDirectory projectDir
  callProcess "git" ["init"]
  callProcess "git" ["add", "."]
  callProcess "git" ["checkout", "-b", "dev"]
  callProcess "git" ["commit", "-m", "Genesis block"]
  
