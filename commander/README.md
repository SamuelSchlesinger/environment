# Commander

The commander package contains two DSLs for describing command line programs, 
one at the type level and one at the term level. The one at the term level looks 
like this:

```
type File = "writer" & Arg "file" FilePath & Arg "contents" FilePath Raw
          + "reader" & Arg "file" FilePath & Raw
```

This is a type which encodes information about an API we want to run. We can
instantiate a term of this type by writing

```
file :: ProgramT File IO
file = sub (arg \file -> arg \contents -> raw $ writeFile file contents) 
   :+: sub (arg \file -> raw $ readFile file >>= putStrLn)
```

I can write a term of this type without specifying the File type by using the
TypeApplications extension.

```
file = sub @"writer" (arg @"file" \file -> arg @"contents" \contents -> raw $ writeFile file contents)
   :+: sub @"reader" (arg @"file" \file -> raw $ readFile file >>= putStrLn)
```


