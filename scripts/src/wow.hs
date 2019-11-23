import System.Environment
import System.Process
import Commander

main :: IO ()
main = command_ . toplevel @"wow" $ raw do
  env <- getEnv "ENVIRONMENT_BASE_PATH"
  callProcess "afplay" [env ++ "/sounds/wow.mp3"]
