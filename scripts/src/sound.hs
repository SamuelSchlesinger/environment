import System.Process
import Commander
import System.Environment

main :: IO ()
main = (command_ . toplevel @"sound")
  (arg @"sound-name" \soundName -> 
    raw do
      env <- getEnv "ENVIRONMENT_BASE_PATH"
      callProcess "afplay" [env ++ "/sounds/" ++ soundName ++ ".mp3"])
