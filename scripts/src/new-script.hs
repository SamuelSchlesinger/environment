import Commander
import System.Environment
import System.Process
import System.Directory
import Data.Text
import Control.Monad
import System.IO

main :: IO ()
main = commander_ . toplevel @"new-script" $ arg @"script-name" @String \scriptName -> raw $ do
  env <- getEnv "ENVIRONMENT_BASE_PATH"
  let scriptFilePath = env ++ "/scripts/src/" ++ scriptName ++ ".hs"
  doesFileExist scriptFilePath >>= \case
    True -> do
      hPutStr stderr ("new-script: script with name " <> scriptName <> " already exists.")
    False -> do
      setCurrentDirectory env
      callProcess "touch" [scriptFilePath]
      writeFile scriptFilePath "main :: IO ()\nmain = pure ()"
      appendFile (env ++ "/scripts/package.yaml") $
        "  " ++ scriptName ++ ":\n\
       \    main: " ++ scriptName ++ ".hs\n\
       \    source-dirs: src\n\
       \    dependencies:\n\
       \      - base\n\
       \      - commander\n\
       \      - process\n\
       \      - directory\n\
       \      - text\n\
       \      - bytestring\n\
       \    <<: *executable\n"
      callProcess "vim" [scriptFilePath]
      callProcess "stack" ["install", "scripts:" ++ scriptName]
