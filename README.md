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
main = commander_ . toplevel @"file" $ (sub @"reader" fileReader <+> sub @"writer" fileWriter) where  
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
    opt @"message-to-print-after" @"print-after" @"p" \printAfter -> raw do
      contents <- readFile fileName
      putStrLn contents
      maybe empty putStrLn printAfter
```

What does this mean? Well, pretty much exactly what it says: I have a top level program named file-program,
which consists of two sub-programs called reader and writer, each of them specified by a
term definition in the where clause. The writer takes two arguments and a flag, and does
some behavior, while the reader takes an argument and an option, writes the second argument
to the file located at the first one, printing out the original contents if the flag is passed.
One thing to note is that currently flags are being parsed with tildes, so that is a different behavior
from most command line parsing libraries. Sorry about that, just being super lazy with the parsing right
now!

Lets say I use this program the wrong way, for instance trying to call "file-program blah".
It will give you back some feedback! At the moment, the feedback is very dumb, just a list
of the valid calls. It is the toplevel function that does this, under the hood using the
help combinator. Here are the results:

```
usage:
file reader <file-name> -p <print-after>
file reader <file-name> --print-after <message-to-print-after>
file writer <file-name> <new-contents> ~print-former-contents
```

I think that these simple help messages are sufficient for everything I ever script for, but if you want
to submit a PR which improves them, please feel free. I can do a discussion of the machinery underlying
the library at some point, but it is modeled off of the [servant](https://www.servant.dev) library at its 
core, but it is much simpler, because it solves a much simpler problem.
