# Environment

This is a project that I use as a vim/Haskell scripting environment. 
To install this environment, one needs the [stack](https://docs.haskellstack.org/)
build tool, and to have the following environment variable set (I have it being set
in my .bashrc file. It can be easily adapted to any other editor if you understand
the Haskell in the script files.
```bash
export ENVIRONMENT_BASE_PATH=<path-of-this-folder>
```
The other thing you have to do is the following command
```bash
stack install scripts:new-script scripts:edit-script
```
The two main scripts are new-script and edit-script, which add and change scripts
to this environment, installing them into your "~/.local/bin" so that you can use
them immediately. The scripting library that you are able to use in these scripts
by default is a simple type level scriping library that I wrote called commander,
which is also located in this repository. This library should be very simple to
get started with. Here is a basic example script of a file reader and a file writer merged
in as subcommands to a larger program:
```haskell
import Commander
import Prelude

main :: IO ()
main = command_ (toplevel @"commander-example" file)

file = sub @"writer" (arg @"filename" \filename ->
                      arg @"contents" \contents ->
                        raw $ writeFile filename contents)
   :+: sub @"reader" (arg @"filename" \filename ->
                        raw $ readFile filename >>= putStrLn)
``` 
Here is a complete example of a script that you could write using
new-script that utilizes most of the combinators in the library.
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
    opt @"p" @"print-after" \printAfter -> raw do
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
usage combinator. Here are the results:
```
usage:
file reader <file-name> -p <print-after>
file writer <file-name> <new-contents> ~print-former-contents
```
I think that these simple help messages are sufficient for everything I ever script for, but if you want
to submit a PR which improves them, please feel free. I can do a discussion of the machinery underlying
the library at some point, but it is modeled off of the [servant](https://www.servant.dev) library at its 
core, but it is much simpler, because it solves a much simpler problem.

Here is an example of a program written with an explicit type signature, and instead of the toplevel
combinator I use the usage combinator explicitly. This is only really useful when you use explicit
type signatures, as otherwise you would have to describe it explicitly at the use site of usage.

```haskell
module Main where

import Commander
import Prelude

type File = Named "file"
          & Arg "filename" FilePath
          & ("write" & Arg "contents" String & Raw
          +  "read"  & Raw)

file :: ProgramT File IO ()
file = named @"file" $ arg \a -> (sub $ arg (raw . writeFile a)) :+: (sub . raw $ readFile a >>= putStrLn)

main :: IO ()
main = command_ (file :+: usage @File)
```
