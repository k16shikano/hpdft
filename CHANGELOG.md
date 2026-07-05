# Changelog

## 0.3.0.0 (2026-07-05)

`master`（0.2.0.0）からの非互換変更を含む。

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
