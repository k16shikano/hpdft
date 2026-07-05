# hpdft (Haskell PDF Tools)

hpdft is a PDF parsing tool and library. It extracts text, metadata, and outlines (table of contents) from PDF files.

## Quick start

```bash
cabal install
hpdft document.pdf              # default: tagged → geometry extraction
hpdft -p 3 document.pdf         # page 3 only
hpdft --legacy document.pdf     # pre-0.3 stream-order extractor
```

## Command usage

```
hpdft [-p|--page PAGE] [-r|--ref REF] [-g|--grep RegExp] [-R|--refs]
             [--geom] [--tagged] [--legacy] [--footnotes] [--ruby]
             [-T|--title] [-I|--info] [-O|--toc] [--trailer]
             [-P|--password PASSWORD] FILE

Available options:
  -p,--page PAGE           Page number (nomble)
  -r,--ref REF             Object reference
  -g,--grep RegExp         grep PDF
  -R,--refs                Show object references in page order
  --geom                   Extract text using geometry-based layout
  --tagged                 Extract text using tagged PDF structure
  --legacy                 Extract text using the pre-0.3 stream-order extractor
  --footnotes              Inline footnote bodies at their anchors as <footnote> tags (geometry pipeline)
  --ruby                   Embed ruby in Aozora bunko notation (geometry/tagged pipeline)
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

## Install

Clone this repository and run cabal-install.

```bash
cabal install
```

## Development

Requires GHC 9.14+ (see `hpdft.cabal`).

```bash
cabal build
cabal test                              # golden (22) + unit (276)
bash scripts/verify_text.sh             # compare all fixture outputs
cabal run interpret-page -- FILE PAGE   # debug glyph positions
```

### Documentation

- [0.3 roadmap](docs/0.3-roadmap.md) — 0.3.0.0 までの architecture, completed phases
- [0.4 roadmap](docs/0.4-roadmap.md) — 0.3.1 ruby, 0.4 API/diff/images plan
- [Changelog](CHANGELOG.md) — release notes

### Library modules (0.3)

| Module | Purpose |
|--------|---------|
| `PDF.Document` | Single-read document handle |
| `PDF.Text` | Text extraction drivers |
| `PDF.Interpret` | Content-stream geometry interpreter |
| `PDF.Layout` | Line/paragraph reconstruction |
| `PDF.Structure` | Tagged PDF logical structure |
| `PDF.Error` | Typed errors and warnings |

## Version

Released: **0.3.1.0** on `feature/0.3.1-ruby` (2026-07-05).
Previous release: **0.3.0.0**.
