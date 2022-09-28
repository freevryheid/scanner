# scanner
text scanner for parsing in fortran

## about
check [scanner.f90](src/scanner.f90) for docs and [main.f90](app/main.f90) for implementation
that replaces spaces for tab indentation and removes excessive spaces from source files

this was originally based on vlang's [textscanner](https://modules.vlang.io/strings.textscanner.html)
but was modified to be more in line with fortran's use of 1 rather than 0 as the start of indices.
Thus when you instantiate a new textscanner the starting position is one (instead of zero)
marking the position of the first character in the string being scanned.
Scanning positions before of after the string [1:len(string)] will return an error (-1).

## build

Add to fpm.toml:
```
[dependencies]
scanner.git = "https://github.com/freevryheid/scanner"
```

fpm build

## test
fpm test
