# Changelog

## 0.4.6.1 (2026-07-06)

### Fixed

- Object streams whose entries mix dictionaries and arrays (e.g. page-tree nodes stored in a single ObjStm) no longer leave intermediate `/Pages` nodes unresolved; page 1 and document order match the PDF catalog again.
- `hpdft.cabal`: `cabal-version` lowered to 3.8 and dependency bounds widened for GHC 9.8 (Hackage docbuilder compatibility).

## 0.4.6.0 (2026-07-05)

### Changed

- **Breaking:** Bare `hpdft FILE` (no subcommand, no mode flags) is now a lightweight viewer: on a TTY it opens an ANSI preview in the lower half of the terminal using fast legacy stream-order extraction; when stdout is a pipe or file it streams legacy text page by page. `hpdft text FILE` keeps the high-quality tagged → geometry batch output on stdout.
- `hpdft text --legacy FILE` now streams page by page (first bytes within about a second on large PDFs) instead of printing one full-document batch; total output is unchanged. Progress `hpdft: page N/M...` goes to stderr on a TTY; suppress with `--quiet`.

### Added

- `PDF.Text.pdfToTextStreamDoc` — legacy extraction with a per-page IO callback (document opened once).
- Self-contained ANSI TUI preview (`TuiPreview`, `TuiScroll`); no brick/vty dependency. Top and bottom border bars, vi-style and arrow-key scrolling, less-style regex search (`/PATTERN`, `n`/`N`) with match highlighting, and East Asian width-aware rendering (IME-friendly search input).

### Fixed

- `hpdft toc` no longer fails with `missing key /Dest in outline` on PDFs whose outline entries use GoTo actions with named destinations (e.g. hyperref output); the name tree under `/Names` → `/Dests` is now resolved, and entries without a resolvable destination keep their title.

## 0.4.5.0 (2026-07-05)

### Changed

- CLI: `extract` subcommand removed; use top-level `text`, `image`, and `form` instead (`hpdft text FILE`, `hpdft image -p PAGE …`, `hpdft form -p PAGE …`)
- `hpdft FILE` with no subcommand runs text extraction (same as `hpdft text FILE`); no deprecation warning for plain text invocation
- Full-document geometry/tagged extraction: streaming per-page layout, Document-level font/stream caches, parallel page interpretation, RTS defaults (`-N -A64m`), and `filterPageGlyphs` band precomputation — `book.pdf` (150 pages) geom ~142s → ~13s (see `dev/performance-0.4.md`)
- Development roadmaps moved from `docs/` to `dev/`; `docs/` is user-facing library guide only

### Added

- `PDF.FormExtract` — extract a named Form XObject from a page to a standalone vector PDF (`pageFormNames`, `extractFormPdf`, `extractFormToFile`)
- Transitive object closure, renumbering, and minimal PDF-1.5 serialization (stream bytes copied as stored in the source index)
- `hpdft form -p PAGE [-n NAME] [-o DIR] [-P PASSWORD] FILE` CLI subcommand; omit `-n` to list top-level Form names on stdout
- English Haddock on primary public API modules (`PDF.Document`, `PDF.Page`, `PDF.Text`, `PDF.Layout`, `PDF.Error`, `PDF.Diff`, `PDF.Image`, `PDF.FormExtract`)
- Example executables `extract-text` and `page-api` under `examples/`

### Documentation

- Library usage guide: `docs/library.md`
- Performance investigation (Japanese): `dev/performance-0.4.md`
- Benchmark scripts: `scripts/bench_book.sh`, `scripts/bench_pages.hs`

### Fixed

- Form extraction no longer double-compresses FlateDecode streams (broken content streams in output PDFs)
- Indirect array objects (e.g. `/DescendantFonts`, OCG `/Intent`) are serialized correctly instead of empty `<< >>` dictionaries
- Unicode strings in copied objects are written as valid PDF hex strings (UTF-16BE)
- Fixture `test/fixtures/form-export-parent.pdf` and unit tests (optional integration test for Fm42 when user PDF is present)

## 0.4.4.0 (2026-07-05)

### Added

- GitHub Actions CI workflow (GHC 9.14.1: build, test, fixture verification)

## 0.4.3.0 (2026-07-05)

### Added

- `PDF.Image` — extract `/Image` XObjects from a page (`extractPageImages`, `extractPageImagesToDir`)
- `hpdft extract images -p PAGE -o DIR FILE` CLI subcommand (JPEG pass-through; DeviceRGB/Gray 8-bit → minimal PNG encoder)
- Nested Form XObject images collected via extended `PDF.Interpret` walk
- Fixture `data/fixtures/jpeg-image.pdf` and unit tests for image extraction and PNG encoding

### Known limitations

- Inline images (`BI` … `EI`) are not extracted in 0.4.3 (planned for a later release)
- Non-JPEG, non–8-bit DeviceRGB/Gray images are written as `.raw` with a JSON sidecar

## 0.4.2.0 (2026-07-05)

### Added

- `PDF.Diff` — paragraph-level text diff between two documents (`compareDocuments`, `diffParagraphs`)
- `hpdft diff FILE_A FILE_B` CLI subcommand with `--json`, `-P`, `--ruby` (and `--geom`/`--legacy` flags)
- `/DCTDecode` stream filter support (JPEG pass-through) in `decodeStreamBytes`
- Filter array cascade and `/ASCII85Decode` in stream decoding
- Unit tests for diff and DCTDecode filter

## 0.4.1.0 (2026-07-05)

### Changed

- CLI refactored into subcommands: `extract`, `info`, `title`, `toc`, `trailer`, `object`, `refs`, `grep`
- Text extraction flags (`-p`, `--geom`, `--tagged`, `--legacy`, `--footnotes`, `--ruby`, `-P`) moved under `hpdft extract`
- Legacy flat-flag invocation (`hpdft FILE`, `hpdft -p 3 FILE`, etc.) retained with a one-time deprecation warning on stderr

### Migration

- Replace `hpdft FILE` with `hpdft extract FILE`
- Replace `hpdft -I FILE` with `hpdft info FILE`, `-T` → `title`, `-O` → `toc`, `-R` → `refs`, `-r` → `object -r`, `-g` → `grep -g`

## 0.4.0.0 (2026-07-05)

### Added

- `PDF.Page` — stable public API for page enumeration and structured extraction
- `pageCount`, `pageRefAt`, `pageItems`, `pageGlyphs`, `pageLines`, `pageParagraphs`, `pageRegions`
- `PageRegion` for per-page paragraph regions (page number, index, bbox, text)
- Unit tests for page API on fixture documents

### Changed

- Page catalog walk centralized in `PDF.Page` (replaces duplicated logic in scripts and `PDF.Text`)
- `interpret-page` script uses `PDF.Page` instead of `DocumentStructure` internals

### Migration

- Prefer `PDF.Page` over direct `DocumentStructure` page-walk helpers in new code
- `PDF.DocumentStructure` remains exposed; no breaking removals in this release

## 0.3.2.0 (2026-07-05)

### Fixed

- `parsePdfNumber` now accepts leading-dot numbers (`.913` → `0.913`, `-.5` → `-0.5`) used by LaTeX/pdflatex `cm` operators; fixes zero glyph size and char-per-line layout fallback
- Type0/CID fonts with `/DescendantFonts` as a direct object reference (not only array-wrapped) now resolve Adobe-Japan1 encoding and descendant `/W` widths
- `codeToUnicode` falls back to Adobe-Japan1-6 when ToUnicode is missing on 2-byte CID fonts
- Glyph advance uses `fiDefaultWidth` when per-code width lookup returns 0
- Coordinate outlier glyphs (e.g. footnote digits at negative y) filtered before line building; fixes spurious paragraph splits
- CJK line-wrap continuation heuristic joins mid-word breaks (e.g. 記/法) without merging distinct paragraphs
- Interleaved ruby/body stream order merged into `base《ruby》` (e.g. 冪等《べきとう》); orphan ruby lines suppressed when `--ruby` is off
- Lettered list markers (`a.`, `b.`), hang-indent bullet items, and list-item boundaries split paragraphs correctly
- Code blocks (numbered lines, small monospace font) extracted with line breaks and x-position indent inference
- ZapfDingbats bullet glyphs (`r` etc.) mapped to `•`; `/ZapfDingbats` encoding recognized

## 0.3.1.0 (2026-07-05)

### Added

- Ruby extraction in Aozora bunko notation (`《…》`, `｜` for mixed-script bases)
- `--ruby` CLI flag (default off); wired through geometry and tagged extraction paths
- `needsAozoraBar`, `aozoraRuby` in `PDF.Layout`
- Tagged PDF `/Ruby` structure parsing (`PDF.Structure.collectRubySpans`)
- Geometry ruby heuristic (horizontal: small line above; vertical: small line to the right)
- Unit tests for ruby detection and Aozora formatting

## 0.3.0.0 (2026-07-05)

Includes backwards-incompatible changes from `master` (0.2.0.0).

### Added

- Structured error model (`PDF.Error`: `PdfError`, `PdfResult`, `PdfWarning`)
- Single-read `Document` API (`PDF.Document`)
- Geometry interpreter (`PDF.Interpret`) with positioned glyphs and graphics marks
- Layout engine (`PDF.Layout`): paragraph heuristics, header/footer removal, cross-page merge, spatial reading order
- Tagged PDF extraction (`PDF.Structure`, MCID tracking, `--tagged`)
- CLI flags: `--geom`, `--tagged`, `--legacy`, `--footnotes`
- Test suites: `hpdft-golden` (11 fixtures × default/legacy), `hpdft-unit` (267 cases)
- Utilities: `interpret-page`, `inspect_font`, Haskell fixture generator

### Changed

- Default text extraction: tagged structure → geometry layout (replaces stream-order walkdown)
- `-p` / `-g` use the geometry pipeline (not legacy ContentStream)
- Dictionaries and indexes use `Map` (sorted output)
- Core PDF object types (`PdfText`, `PdfName`, `PdfHex`, `Dict` keys, `CMap`, `Encoding`) use `Data.Text.Text`
- Dependency `cryptonite` replaced with `crypton`
- Stream reading is `/Length`-driven and binary-safe
- Xref/trailer/object loading returns `PdfResult` instead of crashing

### Fixed

- Hex string tokenizer eating byte after `>` (CID/Type0 Japanese text)
- Tokenizer O(n²) scan on large content streams
- Incremental PDF `/Prev` chain merge order
- RC4/AES decryption, object streams, indirect `/Length`

### Known limitations

- RTL horizontal text unsupported
- `--legacy` retains pre-0.3 stream-order extraction for comparison
- See `dev/0.3-roadmap.md` for details

## 0.2.0.0

- Xref streams, object streams, encryption (R2/R4), incremental updates
- Breaking API changes from 0.1.x
