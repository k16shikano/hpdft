# hpdft (Haskell PDF Tools)

hpdft is a PDF parsing tool. It can also be used as a command to grab text, metadata outline (i.e. table of contents) from PDF. 

Command usage: 

```
hpdft [-p|--page PAGE] [-r|--ref REF] [-g|--grep RegExp] [-R|--refs]
             [-T|--title] [-I|--info] [-O|--toc] [--trailer] FILE

Available options:
  -p,--page PAGE           Page number (nomble)
  -r,--ref REF             Object reference
  -g,--grep RegExp         grep PDF
  -R,--refs                Show object references in page order
  -T,--title               Show title (from metadata)
  -I,--info                Show PDF metainfo
  -O,--toc                 Show table of contents (from metadata)
  --trailer                Show the trailer of PDF
  FILE                     input pdf file
  -h,--help                Show this help text
```

## install

Clone this repository and do cabal-install.

```
$ cabal install
```
