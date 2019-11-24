import Commander
import System.Directory
import System.Process
import System.Environment

data Language = Haskell | Rust

instance Unrender Language where
  unrender "rs" = Just Rust
  unrender "rust" = Just Rust
  unrender "Rust" = Just Rust
  unrender "hs" = Just Haskell
  unrender "haskell" = Just Haskell
  unrender "Haskell" = Just Haskell
  unrender _ = Nothing

main :: IO ()
main = command_ . toplevel @"new-project" 
  $ arg @"project-name" \projectName ->
    opt @"initial-branch-name" @"branch" @"b" \maybeBranchName -> 
    opt @"programming-language" @"language" @"l" \language -> 
      let haskellNewProject =  
            raw do
              let branchName = maybe "dev" id maybeBranchName
              currentDir <- getCurrentDirectory
              env <- getEnv "ENVIRONMENT_BASE_PATH"
              let projectDir = currentDir <> "/" <> projectName
              callProcess "cp" ["-R", env ++ "/project-template", projectDir]
              setCurrentDirectory projectDir
              callProcess "git" ["init"]
              callProcess "git" ["add", "."]
              callProcess "git" ["checkout", "-b", branchName]
              callProcess "git" ["commit", "-m", "Genesis block"]
      in case language of
           Just Haskell -> haskellNewProject
           Just Rust -> raw $ putStrLn "FIXME: new-project does not support rust at the moment"
           Nothing -> haskellNewProject
