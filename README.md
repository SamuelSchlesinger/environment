# Environment

This is a project that I use as a Haskell scripting environment. 
To install this environment, one needs the [stack](https://docs.haskellstack.org/)
build tool, and to have the following environment variable set (I have it being set
in my .bashrc file.

```bash
export ENVIRONMENT_BASE_PATH=<path-of-this-folder>
```

The two main scripts are new-script and edit-script, which add and change scripts
to this environment, installing them into your "~/.local/bin" so that you can use
them immediately. The scripting library that you are able to use in these scripts
by default is a simple type level scriping library that I wrote called commander,
which is also located in this repository. This library should be very simple to
get started with. Here is a complete example of a script that you could write using
new-script that utilizes most of the combinators in the library.:

```haskell
import Commander (arg, opt, flag, raw, toplevel, sub, (<+>))
import Control.Applicative (empty, when)

main :: IO ()
main = commander_ . toplevel @"file-program" $ (sub @"file-reader" fileReader <+> sub @"file-writer" fileWriter) where  
  fileWriter =
    arg @"file-name" \fileName ->
    arg @"new-contents" \newContents ->
    flag @"print-former-contents" \printFormerContents -> raw do
      when printFormerContents $ do
        formerContents <- readFile fileName
        putStrLn formerContents
      writeFile fileName newContents
  fileReader =
    arg @"file-name" \fileName ->
    opt @"print-after" @"print-after" @"p" \printAfter -> raw do
      contents <- readFile fileName
      putStrLn contents
      maybe empty putStrLn printAfter
```

Lets say I use this program the wrong way, for instance trying to call "file-program blah".
It will give you back some feedback! At the moment, the feedback is very dumb, just a list
of the valid calls. It is the toplevel function that does this, under the hood using the
help combinator. Here are the results:

```
usage:
file-program file-reader <file-name> -p <print-after>
file-program file-reader <file-name> --print-after <print-after>
file-program file-writer <file-name> <new-contents> ~print-former-contents
```


