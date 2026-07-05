# hpdft (Haskell PDF Tools)

[![CI](https://github.com/k16shikano/hpdft/actions/workflows/ci.yml/badge.svg)](https://github.com/k16shikano/hpdft/actions/workflows/ci.yml)

hpdft is a PDF parsing tool and library. It extracts text, metadata, and outlines (table of contents) from PDF files.

## Quick start

```bash
cabal install
hpdft document.pdf                 # same as hpdft text (default text extraction)
hpdft text document.pdf            # tagged → geometry extraction
hpdft text -p 3 document.pdf       # page 3 only
hpdft text --legacy document.pdf   # pre-0.3 stream-order extractor
```

`hpdft FILE` (no subcommand) runs text extraction. Legacy flat flags (`hpdft -I FILE`, `hpdft -r REF FILE`, etc.) still work but print a deprecation warning for non-text modes.

## Command usage

```
hpdft [OPTIONS] FILE                      # text extraction (default; same as text)
hpdft text [OPTIONS] FILE                 # text extraction (tagged → geom)
hpdft image -p PAGE -o DIR FILE           # image XObjects from one page
hpdft form -p PAGE FILE                   # list top-level Form names (stdout)
hpdft form -p PAGE -n NAME -o DIR FILE    # extract one Form to standalone PDF
hpdft diff [OPTIONS] FILE_A FILE_B        # paragraph-level diff
hpdft info FILE                           # PDF metadata
hpdft title FILE                          # document title
hpdft toc FILE                            # table of contents
hpdft trailer FILE                        # PDF trailer dictionary
hpdft object -r REF FILE                  # show object by reference
hpdft refs FILE                           # page object references
hpdft grep -g REGEXP FILE                 # search extracted text

Text options:
  -p,--page PAGE           Page number (1-based; 0 = all pages)
  --geom                   Geometry-based layout extraction
  --tagged                 Tagged PDF structure extraction
  --legacy                 Pre-0.3 stream-order extractor
  --footnotes              Inline footnote bodies as <footnote> tags
  --ruby                   Embed ruby in Aozora bunko notation
  -P,--password PASSWORD   Password for encrypted PDF
  FILE                     input pdf file
  -h,--help                Show help text

Image options:
  -p,--page PAGE           Page number (1-based, required)
  -o,--output DIR          Output directory (default: current directory)
  -P,--password PASSWORD   Password for encrypted PDF
  FILE                     input pdf file

Form options:
  -p,--page PAGE           Page number (1-based, required)
  -n,--name NAME           Top-level Form name (e.g. Fm42); omit to list names on stdout
  -o,--output DIR          Output directory (default: current directory)
  -P,--password PASSWORD   Password for encrypted PDF
  FILE                     input pdf file

Diff options:
  --json                   JSON output
  --ruby                   Embed ruby in Aozora bunko notation
  -P,--password PASSWORD   Password for encrypted PDF (both files)
  FILE_A FILE_B            PDF files to compare
```

By default, `hpdft FILE` (or `hpdft text FILE`) extracts text in logical order using the tagged PDF structure when the document has a usable one, and otherwise falls back to geometry-based paragraph reconstruction (equivalent to `--geom`). Use `--legacy` for the pre-0.3 stream-order extractor.

## Library

hpdft is also a Haskell library. See **[docs/library.md](docs/library.md)** for installation, error handling, text pipelines, page API, diff, images, and form extraction.

| Module | Purpose |
|--------|---------|
| `PDF.Document` | Single-read document handle (`openDocument`) |
| `PDF.Error` | `PdfResult`, `PdfError`, `PdfWarning` |
| `PDF.Text` | Text extraction (tagged, geometry, legacy) |
| `PDF.Layout` | `LayoutOptions`, line/paragraph layout |
| `PDF.Page` | Page enumeration and structured extraction |
| `PDF.Diff` | Paragraph-level document comparison |
| `PDF.Image` | Image XObject extraction |
| `PDF.FormExtract` | Form XObject extraction to standalone PDF |
| `PDF.Interpret` | Content-stream geometry interpreter |
| `PDF.Structure` | Tagged PDF logical structure |

Example programs (from repo root):

```bash
cabal run extract-text -- data/fixtures/classic.pdf
cabal run page-api -- data/fixtures/paragraphs.pdf
```

Build Haddock API docs locally:

```bash
cabal haddock --haddock-all
```

## Install

Clone this repository and run cabal-install.

```bash
cabal install
```

## Development

Requires GHC 9.14+ (see `hpdft.cabal`).

```bash
cabal build
cabal test                              # golden (22) + unit
bash scripts/verify_text.sh             # compare all fixture outputs
cabal run interpret-page -- FILE PAGE   # debug glyph positions
```

### Documentation

- [Library guide](docs/library.md) — using hpdft as a Haskell library
- [Changelog](CHANGELOG.md) — release notes

Developer notes (roadmaps, performance write-ups): [`dev/`](dev/)

## Version

Released: **0.4.5.0** (2026-07-05) — Form XObject extraction.
Previous release: **0.4.4.0**.
