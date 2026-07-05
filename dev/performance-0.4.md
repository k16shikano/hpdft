# 全文テキスト抽出の高速化（0.4.5）

2026-07-05。`data/sample/book.pdf`（150 ページ、約 8 MB）を契機に、tagged / geometry パイプラインの全文抽出が実用時間を大きく超える問題を調査し、段階的に対策した記録です。

## 症状

| コマンド | 所要時間（目安） |
|----------|------------------|
| `hpdft --legacy book.pdf` | 約 8 秒 |
| `hpdft -p 1 book.pdf` | 約 0.5 秒 |
| `hpdft --geom book.pdf`（修正前） | **120 秒超**（タイムアウト相当） |
| `hpdft book.pdf`（デフォルト = tagged→geom） | 同上 |

`-p` なしの全文が `-p 1` の 150 倍どころか **300 倍以上** 遅く、legacy より桁違いに遅い。

## 誤解しやすい点：遅延読み込みは壊れていない

0.3 で入れた xref 遅延パース（`buildIndex` の lazy `Map`）は正常に動作している。

- 暗号化 7923 オブジェクトの `test.pdf` で `-p 1` が約 0.8 秒なのは、全オブジェクトを先読みしていない証拠。
- 遅い原因は「オブジェクト index の構築」ではなく、**全文パス固有の処理**にある。

## 原因の切り分け

### 1. 全文と `-p` でパイプラインが違う

- `-p N` → `pageTextGeomWith`（1 ページだけ interpret + layout）
- 全文 → `interpretAllPages` で全 `PageItem` を保持 → `layoutDocumentWith`（document-level のヘッダ除去・ページ跨ぎ段落結合）

ページ数に比例するのは当然だが、150 × 0.5 秒 ≈ 75 秒を大きく超えていた。

### 2. バッチ処理と O(n²) の蓄積

Phase 1（逐次化）以前の全文 geom:

```
openDocument
  → interpretAllPages（全 [[PageItem]] を一括生成・保持）
  → layoutDocumentWith / documentParagraphs
```

`documentParagraphs` 内の `done ++ ...` がページ数・段落数に対して O(n²) だった（`assembleTagged` の `T.append` 連鎖と同型）。

tagged フォールバック時は全ページを **2 回** interpret していた（tagged 判定用 + geom 再実行）。

### 3. ページごとに同じ重い仕事を繰り返す（キャッシュ前）

In-process ベンチマーク（`scripts/bench_pages.hs`、Haskell のみ、Python は使わない）:

| 計測 | 時間 |
|------|------|
| 全 150 ページ interpret+layout 合計 | **134 秒** |
| 重いページ（p50）初回 | 0.76 秒 |
| 同じ p50 を 2 回目以降 | 0.17 秒 |

オブジェクト index は共有されるが、次は **毎ページ再計算** されていた:

- フォントテーブル（`fontInfo` / `findCMap` / ToUnicode CMap パース）
- FlateDecode stream の展開（`rawStreamByRef`）
- 埋め込み OpenType の cmap パース（`noToUnicodeFromDict`）

日本語書籍 PDF はフォント参照数は少なくても CMap が巨大なため、150 回の繰り返しが支配的だった。

### 4. 真のボトルネック（プロファイル後）

キャッシュと並列化後も `--geom` は 40 秒台。GHC プロファイル（`-p -N1`）で判明:

- **`PDF.Layout.filterPageGlyphs` が約 54%**（継承コスト含む）
- 外れ値グリフ除去の `baselineBand` が **グリフ 1 個ごとにページ全グリフを sort** しており、O(n² log n)
- `readToken` / interpret 本体は top 10 外（修正後は主因に移行）

これが「ページ並列化だけでは足りない」「`-p` ループより全文が異常に遅い」主因だった。

## 実施した対策（時系列）

### Step A: 全文 geom の逐次化 + Layout O(n) 化

**変更:** `PDF.Text`, `PDF.Layout`

- `interpretAllPages` 一括 → ページごと `interpretPageLinesRaw` → `PageLines` のみ保持
- `documentParagraphsFromPageLines` を新設、`Data.DList` で `done ++` を O(n) 化
- tagged → geom フォールバック時の二重 interpret を解消

**効果:** 120 秒超・未完 → **約 142 秒で完走**（まだ遅いが hang 解消）。Layout の document-level 上乗せは数秒程度。

### Step B: Document レベルキャッシュ

**変更:** `PDF.Document`, `PDF.Interpret`, `PDF.Image`

`Document` に lazy knot-tied な Map を追加:

```haskell
docStreamCache :: Map Int (PdfResult BSL.ByteString)  -- 展開済み stream
docFontCache   :: Map Int FontInfo                    -- 構築済み FontInfo
```

初回参照時だけ計算し、同一 Document 内では共有。`IState` 経由で interpret が参照。

**効果（book.pdf）:**

| 指標 | Before | After |
|------|--------|-------|
| 全ページ合計（bench_pages） | 136 s | **95 s** |
| `--geom` 全文 | 142 s | **98 s** |

### Step C: ページ並列化

**変更:** `PDF.Text`, `hpdft.cabal`

- `Control.Parallel.Strategies` の `parList` でページ単位 interpret を並列化
- `FontInfo` に関数が含まれるため `NFData` 不可 → `forcePageLines` / `forcePageItems` で手動 seq
- exe / test に `-threaded -rtsopts`

**効果:** geom 98 s → **64 s**（8 コア、150 sparks 全変換）

### Step D: RTS チューニング

**変更:** `hpdft.cabal`

- `-with-rtsopts=-N -A64m`（Gen0 GC が並列実行時に ~20 s 消費していたのを抑制）

**効果:** geom **42 s** 程度。legacy は `-N` だけだと 10 s 台に悪化していたが `-A64m` で **6.8 s** に回復。

### Step E: `filterPageGlyphs` の band 事前計算

**変更:** `PDF.Layout`

- 水平・垂直それぞれ `baselineBand` を **1 回だけ** 計算し、全グリフの membership 判定に再利用
- 出力は golden / diff で **byte-identical** を確認

**効果（最終、book.pdf 150 ページ）:**

| モード | 修正前 | 修正後 |
|--------|--------|--------|
| `--geom` 全文 | 142 s+ | **12.9 s** |
| デフォルト（tagged）全文 | 120 s+ | **13.0 s** |
| `test.pdf` 全文（53 p） | 56 s | **16.3 s** |
| `--legacy` | 6.8 s | 6.8 s |
| `-p 1` | 0.45 s | 0.67 s（`-N` 起動オーバーヘッド） |

## 並列化は有効か

**有効。ただしキャッシュとアルゴリズム修正が先。**

| 順序 | 結果 |
|------|------|
| 並列化のみ（キャッシュ前） | CPU 総量は減らない（全コアでフォント再構築を並列実行するだけ） |
| キャッシュ → 並列化 | 95 s → 64 s |
| 上記 + RTS + filterPageGlyphs | **13 s** |

`interpretPageItems` は `PdfResult` の純粋計算なので `parList` は素直に適用できる。ページ順序は Strategies が保持するため出力は決定的。

## ベンチマークの使い方

```bash
# CLI ベンチ（legacy / geom / -p1）
bash scripts/bench_book.sh

# In-process ベンチ（openDocument 1 回、ページ単価）
cabal exec -- ghc -O2 scripts/bench_pages.hs -o /tmp/bench_pages -outputdir /tmp/bench_pages_obj
/tmp/bench_pages data/sample/book.pdf
```

`data/sample/book.pdf` は `.gitignore` 対象。手元に配置して使う。

## 残課題

1. **interpret トークナイザ** — プロファイル上、Step E 後は ByteString 走査が次の主因。全面書き換えはリスク大。
2. **library の `-O1`** — exe のみ `-O2`。library も `-O2` にするとさらに短縮の余地あり。
3. **`-p 1` の微増** — `-N` スレッド起動コスト。気になる場合は RTS を環境変数で `-N1` に限定可能。
4. **UX** — 全文の TTY 表示は legacy 逐次 + TUI、ファイル出力は tagged→geom。方針は [0.5-text-ux.md](0.5-text-ux.md)。

## 関連ファイル

| パス | 役割 |
|------|------|
| `src/PDF/Document.hs` | `docStreamCache`, `docFontCache` |
| `src/PDF/Text.hs` | 逐次 geom、並列 `parList` |
| `src/PDF/Layout.hs` | `filterPageGlyphs` 修正、DList、`layoutDocumentFromPageLines` |
| `src/PDF/Interpret.hs` | キャッシュ参照、`forcePageItems` 向け |
| `scripts/bench_book.sh` | CLI 計測 |
| `scripts/bench_pages.hs` | In-process 計測 |
