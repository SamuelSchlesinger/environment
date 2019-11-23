# commander

Commander is a command line interface builder similar to the servant library
for building webservers. There are a few ways you can use commander. Here is
some code that takes in an filepath and prints the contents of it out to standard
output:

```haskell
import Commander (commander_, toplevel, arg)
import Control.Monad (>=>)

main :: IO ()
main = commander_ . toplevel @"file-reader" $ arg @"filepath" (readFile >=> putStrLn)
```

What this is doing is building a commandline program that is called file-reader, takes
an argument called filepath, and reads a file and then prints its contents to standard
out. If you fail to pass in a properly formatted argument (in this case it is a string
so that isn't very likely) the toplevel function ensures that running your program will
result in a mildly informative help message, a list of all of the subcommands and their
various options and flags.

We can declare the type of our command line program explicitly as follows:

```haskell
type FileReader = Arg "filepath" String ... Raw
```

When we run our program through toplevel, we automatically get this minimalistic
documentation, so it is encouraged to at least start with that so you can use your interfaces
more usably. We can see the type of toplevel in Commander.hs:

```haskell
toplevel :: forall s p. (HasProgram p, KnownSymbol s, MonadIO m) 
         => ProgramT p m
         -> ProgramT (Named s ... p + Raw) m
```

This allows us to use a type parameter "file-reader" to name our program at the top level.
The ability to write these programs inline and get interpreters for them automatically is
nice, and it is all typesafe as well! I have transitioned to using this for my own scripting,
as I have a number of scripts that I interact with a lot and edit regularly, and they
all had ad-hoc methods for dealing with their arguments, documentation, etc.
