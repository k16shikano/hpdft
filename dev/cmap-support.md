# 定義済み CMap の対応状況と追加対応の優先度

2026-07-06 時点の調査メモ。90ms-RKSJ 対応（Shift-JIS 系定義済み CMap の文字化け修正）の際に、残りの定義済み CMap をどこまで対応すべきか整理した。

## 現状の対応範囲

| 経路 | 対応内容 |
|------|----------|
| `/ToUnicode` あり | CMap ストリームをパースして CID→Unicode（最優先。近年の PDF はほぼこれで足りる） |
| `/Identity-H` + Ordering Japan1 | `CIDmap "Adobe-Japan1"` → `data/map/Adobe-Japan1-6.map`（CID→UTF-8、file-embed） |
| `/Identity-H` + その他 Ordering | `CIDmap _` → **`safeChr` フォールバック（化ける）** |
| `/90ms-RKSJ-H/V`, `/90msp-RKSJ-H/V`, `/RKSJ-H/V` | `SJISmap` → `data/map/cp932.map`（SJIS→Unicode 直接変換、CID 非経由） |
| `/UniJIS-UCS2-H/V`, `/UniJIS-UCS2-HW-H/V`, `/UniJIS-UTF16-H/V`, `/UniJIS2004-UTF16-H/V` | `UnicodeMap`（UTF-16BE 可変長分割、コード値 = Unicode） |
| `/H`, `/V`（完全一致） | `JISmap` → `data/map/jisx0208.map`（JIS X 0208 区点 + 0x21 → Unicode） |
| 上記以外の定義済み CMap 名 | `NullMap` → 2 バイト固定ペア + Adobe-Japan1 引き + `safeChr`（化ける） |

幅について: `SJISmap` はコード→CID 変換を持たないため `/W` を引けず `/DW`（既定 1000）で代用している。全角主体の文書では実害は小さい。

## 問題になる条件

定義済み CMap 対応が必要になるのは「**`/ToUnicode` なし**」の場合だけ。そのうえで:

1. `/Encoding` が Identity 以外の定義済み CMap 名 → コード体系の解釈が必要
2. `/Encoding` が Identity で Ordering が Japan1 以外（GB1/CNS1/Korea1）→ CID→Unicode マップが必要

## 追加対応の優先度

### P1: UniJIS 系（Unicode ベース、テーブル不要） — **実装済み**

`UniJIS-UCS2-H/V`, `UniJIS-UCS2-HW-H/V`, `UniJIS-UTF16-H/V`, `UniJIS2004-UTF16-H/V`

- dvipdfmx + japanese-otf パッケージの定番（フォントマップに `UniJIS2004-UTF16-H` などが直に書かれる）。TeX 由来の日本語 PDF で遭遇率が高い
- **実装が最も軽い**: コード = Unicode コードポイントそのもの。UCS2 は 2 バイト固定、UTF16 はサロゲートペアの可変長分割のみ
- 方針: `Encoding` に `UnicodeMap`（UTF-16BE 解釈）を追加。`bytesToCodes` / `hexStringToCodes` にサロゲート対応の分割を実装。変換テーブル不要
- `UniJIS-UCS2-HW` の HW（半角幅）は幅情報のみの差なので Unicode 変換としては同一扱いでよい
- テスト: `data/fixtures/cmap-unijis.pdf`（golden）、`test/Unit.hs` の UTF-16BE 分割テスト

### P1: H / V（ISO-2022-JP、JIS X 0208 区点 + 0x21） — **実装済み**

- dvipdfmx の 90JIS 設定（`rml H` / `rmlv V`）で使われる。pTeX 標準の和文フォントはこれ。TeX 由来 PDF での遭遇率が高い
- 2 バイト固定。JIS→Unicode テーブルが必要
- 方針: `JISmap` を追加。テーブルは EUC-JP 変換表から生成（区点の両バイトに 0x80 を足して EUC-JP としてデコード）し、`data/map/jisx0208.map` として file-embed。構造は SJISmap と同じ
- `/V` の縦書きは `wmodeFromEncoding` に `/V` 完全一致ケースを追加（`-V` サフィックス判定だけでは `/V` にマッチしない）
- テスト: `data/fixtures/cmap-jis-h.pdf`（golden）、`test/Unit.hs` の JIS コード→Unicode テスト

### P2: EUC-H / EUC-V

- UNIX 系ワークフロー由来の古い PDF。遭遇率は 90ms-RKSJ や H より低い
- 可変長: 0xA1–0xFE は 2 バイト、0x8E は半角カナ（2 バイト目が実体）、それ以外 1 バイト
- 方針: `EUCmap` を追加、テーブルは `euc_jp` コーデックから生成。P1 の JIS テーブルと生成元が同じなので同時に実装すると安い

### P2: SJIS 亜種（83pv-RKSJ-H, 90pv-RKSJ-H, Add-RKSJ-H/V, Ext-RKSJ-H/V）

- Mac 由来（pv 系）・NEC/IBM 拡張（Add/Ext）の Shift-JIS 亜種。差分はベンダー外字領域のみで、基本領域は cp932 と同一
- 方針: まず **既存 `SJISmap` へのエイリアス**として名前リストに追加するだけ（1 行×6 名）。外字部分の差異は許容。完全対応が必要になったら個別テーブルを検討

### P2: Adobe-GB1 / Adobe-CNS1 / Adobe-Korea1(KR) の CID→Unicode マップ

- 「Identity-H + 中国語/韓国語フォント + ToUnicode なし」で化ける問題への対応。`encodingUnicode (CIDmap _) = safeChr` のフォールバックを置き換える
- 方針: Adobe が公開する cid2code.txt（[adobe-type-tools/cmap-resources](https://github.com/adobe-type-tools/cmap-resources)）から `Adobe-Japan1-6.map` と同形式で生成し file-embed。`cidmapToList.hs`（data/map/ に生成器あり）を流用できる可能性が高い
- 注意: バイナリサイズ増（Adobe-Japan1-6.map は 95KB。GB1/CNS1 は同規模かやや大きい）。3 つ足すと +300KB 程度になる見込み。実需要（issue 報告など）が出てからでもよい

### P3: 中国語・韓国語のバイトコード系 CMap

`GBK-EUC-H/V`, `GBKp-EUC-H/V`, `GB-EUC-H/V`（簡体字・GBK/cp936）、`ETen-B5-H/V`, `B5pc-H/V`, `HKscs-B5-H/V`（繁体字・Big5/cp950）、`KSCms-UHC-H/V`, `KSC-EUC-H/V`（韓国語・cp949）、`UniGB-*`, `UniCNS-*`, `UniKS-*`（Unicode 系）

- 実装パターンは日本語と同じ（Uni 系はテーブル不要、それ以外は各コードページの変換表からテーブル生成 + 可変長分割）
- 現状ユーザーの需要が見えないので、P2 の CID→Unicode マップとあわせて要望が出てから

### P3: 幅の正確化（コード→CID テーブル）

- `SJISmap`（と今後の JIS/EUC 系）は CID を経由しないため `/W` の幅を引けず `/DW` 代用
- 正確にやるには Adobe の CMap ファイル（90ms-RKSJ-H 等、コード→CID 定義）の埋め込みが必要
- 全角主体の日本語文書では実害が小さいので、レイアウト品質の問題が具体的に出てから

## 実装の共通パターン

新しいコード系 CMap を 1 つ足す手順（90ms-RKSJ 実装で確立済み）:

1. `src/PDF/Definition.hs`: `Encoding` にコンストラクタ追加（手書き `Show` インスタンスも）
2. `src/PDF/DocumentStructure.hs`: `encodingFromDict` の Type0 ケースに名前リスト追加。縦書きは `wmodeFromEncoding` が `-V` サフィックスで判定済みなので追加作業なし
3. コード分割: `src/PDF/Interpret.hs` の `bytesToCodes`、`src/PDF/ContentStream.hs` の `hexStringToCodes`（+ リテラル文字列側）に可変長分割を追加
4. Unicode 変換: `Interpret.hs` の `encodingUnicode`、`ContentStream.hs` の `lookupUcs` 経路にテーブル参照を追加
5. テーブルが必要なら変換表から生成して `data/map/*.map` に置き `src/PDF/Character.hs` で file-embed（gzip + `Data.Binary` の `Map Int ByteString` 形式）。生成スクリプトはリポジトリには含めない（`data/map/cidmapToList.hs` のように Haskell で書く場合のみ可）

## テストの課題

- `data/sample/` の実 PDF はローカルのみ（.gitignore）で CI に乗らない
- ToUnicode なし + 各 CMap の最小フィクスチャ PDF を `data/fixtures/` に生成して golden テストに足すのが望ましい（生成スクリプトから作れば再配布問題なし）。P1 実装時にあわせて整備する
