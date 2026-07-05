# hpdft (Haskell PDF Tools)

hpdft is a PDF parsing tool. It can also be used as a command to grab text, metadata outline (i.e. table of contents) from PDF. 

Command usage: 

```
hpdft [-p|--page PAGE] [-r|--ref REF] [-g|--grep RegExp] [-R|--refs]
             [--geom] [--tagged] [--legacy] [-T|--title] [-I|--info]
             [-O|--toc] [--trailer] [-P|--password PASSWORD] FILE

Available options:
  -p,--page PAGE           Page number (nomble)
  -r,--ref REF             Object reference
  -g,--grep RegExp         grep PDF
  -R,--refs                Show object references in page order
  --geom                   Extract text using geometry-based layout
  --tagged                 Extract text using tagged PDF structure
  --legacy                 Extract text using the pre-0.3 stream-order extractor
  --footnotes              Inline footnote bodies at their anchors as <footnote> tags (geometry pipeline)
  -T,--title               Show title (from metadata)
  -I,--info                Show PDF metainfo
  -O,--toc                 Show table of contents (from metadata)
  --trailer                Show the trailer of PDF
  -P,--password PASSWORD   Password for encrypted PDF
  FILE                     input pdf file
  -h,--help                Show this help text
```

By default, `hpdft FILE` extracts text in logical order using the tagged
PDF structure when the document has a usable one, and otherwise falls
back to geometry-based paragraph reconstruction (equivalent to `--geom`).
Use `--legacy` for the pre-0.3 stream-order extractor.

## install

Clone this repository and do cabal-install.

```
$ cabal install
```
