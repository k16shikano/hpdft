# hpdft (Haskell PDF Tools)

hpdft is a PDF parsing tool and library. It extracts text, metadata, and outlines (table of contents) from PDF files.

## Quick start

```bash
cabal install
hpdft extract document.pdf           # default: tagged → geometry extraction
hpdft extract -p 3 document.pdf      # page 3 only
hpdft extract --legacy document.pdf  # pre-0.3 stream-order extractor
```

Legacy flat-flag invocation (`hpdft document.pdf`, `hpdft -p 3 document.pdf`) still works but prints a deprecation warning; prefer the subcommands below.

## Command usage

```
hpdft extract [OPTIONS] FILE              # text extraction (tagged → geom)
hpdft extract text [OPTIONS] FILE         # explicit text extraction
hpdft info FILE                           # PDF metadata
hpdft title FILE                          # document title
hpdft toc FILE                            # table of contents
hpdft trailer FILE                        # PDF trailer dictionary
hpdft object -r REF FILE                  # show object by reference
hpdft refs FILE                           # page object references
hpdft grep -g REGEXP FILE                 # search extracted text

Extract options:
  -p,--page PAGE           Page number (1-based; 0 = all pages)
  --geom                   Geometry-based layout extraction
  --tagged                 Tagged PDF structure extraction
  --legacy                 Pre-0.3 stream-order extractor
  --footnotes              Inline footnote bodies as <footnote> tags
  --ruby                   Embed ruby in Aozora bunko notation
  -P,--password PASSWORD   Password for encrypted PDF
  FILE                     input pdf file
  -h,--help                Show help text
```

By default, `hpdft extract FILE` extracts text in logical order using the tagged PDF structure when the document has a usable one, and otherwise falls back to geometry-based paragraph reconstruction (equivalent to `--geom`). Use `--legacy` for the pre-0.3 stream-order extractor.

## Install

Clone this repository and run cabal-install.

```bash
cabal install
```

## Development

Requires GHC 9.14+ (see `hpdft.cabal`).

```bash
cabal build
cabal test                              # golden (22) + unit (279)
bash scripts/verify_text.sh             # compare all fixture outputs
cabal run interpret-page -- FILE PAGE   # debug glyph positions
```

### Documentation

- [0.3 roadmap](docs/0.3-roadmap.md) — 0.3.0.0 までの architecture, completed phases
- [0.4 roadmap](docs/0.4-roadmap.md) — 0.3.1 ruby, 0.4 API/diff/images plan
- [Changelog](CHANGELOG.md) — release notes

### Library modules (0.4)

| Module | Purpose |
|--------|---------|
| `PDF.Document` | Single-read document handle |
| `PDF.Page` | Page enumeration and structured extraction |
| `PDF.Text` | Text extraction drivers |
| `PDF.Interpret` | Content-stream geometry interpreter |
| `PDF.Layout` | Line/paragraph reconstruction |
| `PDF.Structure` | Tagged PDF logical structure |
| `PDF.Error` | Typed errors and warnings |

## Version

Released: **0.4.1.0** on `feature/0.4-api` (2026-07-05).
Previous release: **0.4.0.0**.
