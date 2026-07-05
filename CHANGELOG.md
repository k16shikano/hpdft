# Changelog

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
- See `docs/0.3-roadmap.md` for details

## 0.2.0.0

- Xref streams, object streams, encryption (R2/R4), incremental updates
- Breaking API changes from 0.1.x
