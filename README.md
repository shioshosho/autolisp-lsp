# autolisp-lsp

AutoCAD(およびBricsCAD)で使用されるAutoLISP言語のLanguage Server Protocol(LSP)サーバー

## 機能

| 機能 | LSP メソッド | 説明 |
|------|-------------|------|
| Go to Definition | textDocument/definition | 関数名からdefun定義へジャンプ、クロスファイル対応 |
| Find References | textDocument/references | シンボル使用箇所を一覧表示 |
| Document Highlight | textDocument/documentHighlight | カーソル下のシンボルを同一ファイル内でハイライト (定義=Write / 参照=Read) |
| Hover | textDocument/hover | 組み込み関数はシグネチャ+説明、ユーザー定義はdefunシグネチャ+doc |
| Completion | textDocument/completion | 組み込み関数、ユーザー定義関数/変数、他ファイルの関数を補完 |
| Signature Help | textDocument/signatureHelp | 関数呼び出し中にパラメータ名とアクティブ引数をリアルタイム表示 |
| Formatting | textDocument/formatting | ドキュメント全体を2スペースインデントでフォーマット |
| Range Formatting | textDocument/rangeFormatting | 選択範囲のみフォーマット |
| Diagnostics | textDocument/publishDiagnostics | パースエラーをリアルタイム検出・表示 |

## ビルド

```sh
cargo build --release
```

target/releaseにautolisp-lspバイナリが生成される

## 使い方

LSPクライアントからstdio経由で接続

### VSCode

settings.jsonで以下のようにサーバーを指定:

```json
{
  "autolisp.lsp.serverPath": "/path/to/autolisp-lsp"
}
```

### Neovim (nvim-lspconfig)

```lua
vim.api.nvim_create_autocmd("FileType", {
  pattern = "lisp",  -- .lsp ファイルを lisp filetype にマッピング
  callback = function()
    vim.lsp.start({
      name = "autolisp-lsp",
      cmd = { "/path/to/autolisp-lsp" },
    })
  end,
})
```

## アーキテクチャ

```
src/
├── main.rs                  サーバー起動
├── server.rs                Backend + LanguageServer trait実装
├── document.rs              テキスト・行オフセット・AST・シンボルテーブル
├── builtins.rs              組み込み関数DB
├── parser/
│   ├── token.rs             Token列挙型 + Span型
│   ├── lexer.rs             レキサー(トークナイザー)
│   ├── ast.rs               ASTノード型
│   ├── parser.rs            再帰下降パーサー
│   └── mod.rs               
├── analysis/
│   ├── symbol_table.rs      SymbolTable / SymbolInfo / SymbolReference
│   ├── analyzer.rs          AST→シンボル収集 (defun / setq / foreach)
│   └── mod.rs
└── features/
    ├── definition.rs        Go to Definition
    ├── references.rs        Find References
    ├── highlight.rs         Document Highlight
    ├── hover.rs             Hover
    ├── completion.rs        Completion
    ├── signature.rs         Signature Help
    └── formatting.rs        Formatting / Range Formatting
```

## 工夫点

### 自作再帰下降パーサー

パーサージェネレーター(tree-sitterなど)ではなく自作の再帰下降パーサーを採用

- エラー耐性の完全制御
  - 閉じ括弧が欠落していても以降の式がパース可能
- defunパターンの直接認識
  - (defun name (params / locals) ...)を専用ASTノードとしてパースし、後段の解析を単純化
- AutoLISP の文法の単純さ
  - S式ベース
    - 単純なのでBNF(Backus-Naur Form)が数行に収まる
      - パーサージェネレーターのオーバーヘッドのほうが大きい

### 大文字正規化によるcase-insensitive比較

AutoLISPは大文字小文字を区別しないため(SETQ = setq)、Lexerの段階で全シンボルを大文字に正規化し、シンボルテーブルの検索を単純な文字列比較にしている

### DashMapによるlock-free並行アクセス

- tower-lspのハンドラは各リクエストを並行に処理するため、ドキュメントストアにDashMapを使うことでRwLock<HashMap>のような大きなロックを避けている
- これにより複数のリクエストが同時にデータへアクセスしてもパフォーマンス低下や競合を最小限に抑えられる

### 組み込み関数DBの静的コンパイル

関数情報 (シグネチャ、引数、説明、カテゴリ) を静的データとしてバイナリに埋め込むことで外部 JSONファイルの読み込みを不要とし、パフォーマンス向上に寄与させた

### 変更時の全文再パース

AutoLISPファイルは長くても5000行には収まると考えられるので(主観)、毎回全文パースを行って正確性を優先

## Rustの採用理由

### vs TypeScript

| 観点 | TypeScript | Rust |
|------|-----------|------|
| ランタイム依存 | Node.js依存 | 単一バイナリのため依存なし |
| 起動速度 | Node.js の起動+諸々で数百ms | バイナリで即時起動 |
| メモリ | V8ヒープのベースだけで数十MB | 全て含めても数MB |
| 配布 | node_modulesを含むと数十MB | 数MB の単一ファイル |

### vs Go

| 観点 | Go | Rust |
|------|------|------|
| ADT (代数的データ型) | ないのでAST表現はinterfaceと型アサーションで構成 | enumでパースエラーも含めたASTを表現可能 |
| パターンマッチ | switch文、網羅性チェックなし | match文、網羅性チェックあり |
| エラーハンドリング | if err != nilの反復 | Result型で簡潔 |
