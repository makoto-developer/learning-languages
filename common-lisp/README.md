# Learning Common-Lisp

## 環境仕様

- **SBCL**: 2.5.10 (Steel Bank Common Lisp)
- **ベースイメージ**: Alpine Linux 3.22
- **Quicklisp**: インストール済み

### Alpine版を採用する理由

✅ **セキュリティ**: Debian版と比較して脆弱性が大幅に少ない（0 CVE vs 22+ CVE）
✅ **軽量**: イメージサイズが小さい（約5MB）
✅ **最小構成**: 必要最小限のパッケージのみ

## Install

### Docker環境（推奨）

#### 1. 環境変数の設定

```shell
# .env.exampleをコピー
cp .env.example .env

# 必要に応じて.envを編集
# - SBCL_VERSION: SBCLバージョン（Alpine版のタグ）
# - COMPOSE_PROJECT_NAME: プロジェクト名
# - CONTAINER_NAME: コンテナ名
# - MEMORY_LIMIT: メモリ制限
# - CPU_LIMIT: CPU制限
# - USER_ID: ユーザーID（ホストと合わせる場合）
```

#### 2. コンテナの起動

```shell
# コンテナ起動（初回はビルドも実行）
docker compose up -d

# REPL起動
docker compose exec sbcl sbcl

# ファイルを読み込んで実行
docker compose exec sbcl sbcl --load src/hello.lisp

# コンテナ停止
docker compose down
```

### 使い方

REPLでの実行例:

```lisp
;; ファイル読み込み
(load "src/hello.lisp")

;; 関数実行
(hello)
(greet "World")
```

## セキュリティ設定

### 実装済みのセキュリティ対策

- ✅ 非rootユーザー（lispuser）で実行
- ✅ `no-new-privileges` 設定
- ✅ 不要なLinux capabilityをdrop
- ✅ メモリ・CPU制限
- ✅ Alpine Linuxで脆弱性を最小化
- ✅ `.env`ファイルはGit管理対象外

## 言語仕様

