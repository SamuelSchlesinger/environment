import Commander
import System.Environment
import System.Directory
import Data.Maybe

main :: IO ()
main = command_ . toplevel @"list-scripts" $ raw do
  env <- getEnv "ENVIRONMENT_BASE_PATH"
  setCurrentDirectory env
  scripts <- (mapMaybe scriptFilter . map words . lines) <$> readFile "scripts/package.yaml"
  mapM_ putStrLn scripts where
    scriptFilter = \case
      "main:":["Test.hs"] -> Nothing
      "main:":[script] -> Just (takeWhile (/= '.') script)
      _ -> Nothing
