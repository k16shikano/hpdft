# hpdft library guide

hpdft is a Haskell library (and CLI) for parsing PDF files: text extraction with geometry and tagged structure, page-level paragraph regions, document diff, image export, and Form XObject extraction.

## Installation

Add a dependency on Hackage (when published):

```cabal
build-depends: hpdft >= 0.4.5
```

Or build from this repository:

```bash
cabal install
# library + hpdft CLI + examples
```

Requires GHC 9.14+ (see `hpdft.cabal`).

## Opening a document

Load a file once and reuse the parsed `Document` for every operation:

```haskell
import PDF.Document (openDocument)
import PDF.Error (PdfError(..), PdfResult)

main :: IO ()
main = do
  result <- openDocument "paper.pdf" Nothing
  case result of
    Left err  -> print err
    Right doc -> ...
```

For encrypted PDFs, pass `Just password` as the second argument. Failures (broken xref, missing objects, bad password) are returned as `Left` values of type `PdfError` in `PdfResult` (`Either PdfError a`) instead of raising exceptions.

## Text extraction

### Default pipeline (tagged → geometry)

The CLI default (`hpdft FILE` or `hpdft text FILE`) matches `pdfToTextTaggedDoc`:

```haskell
import PDF.Text (pdfToTextTaggedDoc)
import qualified Data.ByteString.Lazy.Char8 as BSL

case pdfToTextTaggedDoc doc of
  Left err -> print err
  Right bs -> BSL.putStr bs
```

When the PDF has a usable tagged structure tree, text is assembled in logical order. Otherwise hpdft falls back to geometry-based paragraph reconstruction (same as `pdfToTextGeomDoc`).

### Geometry-only

```haskell
import PDF.Text (pdfToTextGeomDoc)
```

Equivalent to `hpdft text --geom FILE`.

### Tagged-only attempt

Use `pdfToTextTaggedDocWith` / `pdfToTextTaggedBSWith` with `LayoutOptions`. When no structure tree exists, these still fall back to geometry.

### Legacy stream-order extractor

```haskell
import PDF.Text (pdfToTextDoc)
```

Returns `(ByteString, [PdfWarning])` directly (not `PdfResult`). Same output style as `hpdft text --legacy FILE`. Prefer the geometry or tagged paths for new code.

### Layout options

Footnotes and ruby (Aozora bunko notation) are controlled by `PDF.Layout.LayoutOptions`:

```haskell
import PDF.Layout (LayoutOptions(..), defaultLayoutOptions)

opts = defaultLayoutOptions { optRuby = True, optFootnotes = False }
```

Pass `opts` to `pdfToTextGeomDocWith`, `pdfToTextTaggedDocWith`, page APIs, and diff.

## Page-level API

`PDF.Page` exposes stable page enumeration and structured extraction:

| Function | Role |
|----------|------|
| `pageCount` | Number of pages |
| `pageRefAt` | Resolve 1-based page number to internal page reference |
| `pageItems` | Raw interpreted page items (glyphs, graphics) |
| `pageGlyphs` | Glyph list for one page |
| `pageLines` | Layout lines with `LayoutOptions` |
| `pageParagraphs` | Paragraph texts for one page |
| `pageRegions` | Paragraph text plus bounding box and indices |

Example:

```haskell
import PDF.Layout (defaultLayoutOptions)
import PDF.Page (pageCount, pageRefAt, pageRegions)

n <- pageCount doc
ref <- pageRefAt doc 1
regions <- pageRegions doc ref defaultLayoutOptions
```

See `examples/page-api/Main.hs` for a runnable program.

## Document diff

`PDF.Diff.compareDocuments` aligns pages by number and diffs paragraph text:

```haskell
import PDF.Diff (compareDocuments, TextChange(..))
import PDF.Layout (defaultLayoutOptions)

changes <- compareDocuments defaultLayoutOptions docA docB
```

Each `TextChange` records old/new paragraph text and optional page/paragraph indices. A `PageCountMismatch` entry appears when page counts differ.

CLI equivalent: `hpdft diff FILE_A FILE_B`.

## Image extraction

`PDF.Image.extractPageImages` returns `/Image` XObjects on a page (including nested forms):

```haskell
import PDF.Image (extractPageImages, extractPageImagesToDir)

images <- extractPageImages doc pageNum
paths  <- extractPageImagesToDir doc pageNum "out/"
```

JPEG streams are passed through; 8-bit DeviceRGB/Gray become minimal PNG. Inline images (`BI` … `EI`) are not extracted yet.

## Form extraction

`PDF.FormExtract` exports a named Form XObject to a standalone vector PDF:

```haskell
import PDF.FormExtract (pageFormNames, extractFormPdf, extractFormToFile)

names <- pageFormNames doc pageNum
bytes <- extractFormPdf doc pageNum "Fm42"
path  <- extractFormToFile doc pageNum "Fm42" "out/"
```

`pageFormNames` lists top-level form names on the page resources (not nested forms).

## Example programs

From the repository root:

```bash
cabal run extract-text -- data/fixtures/classic.pdf
cabal run page-api -- data/fixtures/paragraphs.pdf
```

With no argument, each example walks up from the current directory to find `data/fixtures/…`.

Sources: `examples/extract-text/Main.hs`, `examples/page-api/Main.hs`.

## API reference (Haddock)

Generate HTML documentation locally:

```bash
cabal haddock --haddock-all
```

Open `dist/doc/html/hpdft/index.html` (path may vary slightly by cabal version).

Primary public modules:

| Module | Purpose |
|--------|---------|
| `PDF.Document` | `openDocument`, `Document` |
| `PDF.Error` | `PdfResult`, `PdfError`, `PdfWarning` |
| `PDF.Text` | Full-document text extraction |
| `PDF.Layout` | `LayoutOptions`, paragraph heuristics |
| `PDF.Page` | Page count, paragraphs, regions |
| `PDF.Diff` | Paragraph-level diff |
| `PDF.Image` | Image XObject extraction |
| `PDF.FormExtract` | Form XObject export |

Lower-level modules (`PDF.Interpret`, `PDF.Structure`, `PDF.DocumentStructure`, …) are exposed for advanced use but may change more often.

## Known limitations

- RTL horizontal text is unsupported.
- Inline images are not extracted (see CHANGELOG 0.4.3).
- Some image color spaces are written as `.raw` plus JSON sidecar.
- Legacy stream-order extraction remains for comparison only.
- Details and roadmap: [CHANGELOG](../CHANGELOG.md). Developer notes: [dev/](../dev/).
