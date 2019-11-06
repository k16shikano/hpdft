# hpdft (Haskell PDF Tools)

hpdft is a PDF parsing tool. It can also be used as a command to grab text, metadata outline (i.e. table of contents) from PDF. 

Command usage: 

```
hpdft [-p|--page PAGE] [-r|--ref REF] [-R|--refs] [-T|--title]
             [-I|--info] [-O|--toc] FILE

Available options:
  -p,--page PAGE           Page number
  -r,--ref REF             Object reference
  -R,--refs                Show object references in page order
  -T,--title               Show title (from metadata)
  -I,--info                Show PDF metainfo
  -O,--toc                 Show table of contents (from metadata)
  FILE                     input pdf file
  -h,--help                Show this help text
```

## install

```
$ cabal install
```
