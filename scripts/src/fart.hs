import Commander
import System.Process
import System.Environment

main :: IO ()
main = command_ . toplevel @"fart" $ raw $ do
  env <- getEnv "ENVIRONMENT_BASE_PATH"
  callProcess "afplay" [env ++ "/sounds/fart.mp3"]
