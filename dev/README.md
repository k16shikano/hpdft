# 開発者向けメモ

このディレクトリは **hpdft の開発・設計メモ** 用です。利用者向けの解説は [`docs/`](../docs/) のみに置きます。

| ファイル | 内容 |
|----------|------|
| [performance-0.4.md](performance-0.4.md) | 0.4.5 前後の全文テキスト抽出の高速化（調査・対策・計測） |
| [0.3-roadmap.md](0.3-roadmap.md) | 0.3.0.0 までの開発ロードマップ（完了） |
| [0.4-roadmap.md](0.4-roadmap.md) | 0.4 系 API / diff / images / form の計画 |
| [0.5-text-ux.md](0.5-text-ux.md) | CLI 出力 UX（TUI プレビューは 0.4.6 で実装済み / `-o` は 0.5 へ持ち越し） |
| [0.5-roadmap.md](0.5-roadmap.md) | 0.5 計画: layout モード、TUI 高さ指定（0.4.7 で整備済み）、D 節新機能 |
| [cmap-support.md](cmap-support.md) | 定義済み CMap の対応状況と追加対応の優先度 |

ベンチマーク用 PDF（`data/sample/book.pdf` など）は `.gitignore` 対象のためリポジトリには含めません。手元に置いて `scripts/bench_book.sh` を実行してください。
