# Learning Common-Lisp

## 環境仕様

- **SBCL**: 2.5.10 (Steel Bank Common Lisp)
- **ベースイメージ**: Alpine Linux 3.22
- **Quicklisp**: インストール済み

### Alpine版を採用する理由

✅ **セキュリティ**: Debian版と比較して脆弱性が大幅に少ない（0 CVE vs 22+ CVE）
✅ **軽量**: イメージサイズが小さい（約5MB）
✅ **最小構成**: 必要最小限のパッケージのみ

## クイックスタート（Makefileを使う）

### 初回セットアップ

```shell
# 1. .envファイルを作成
make init

# 2. （重要）Jupyter用のトークンを設定
# ⚠️ セキュリティ必須: 強力なランダムトークンを生成
openssl rand -hex 32
# 👆 出力された64文字の文字列をコピー

vi .env
# JUPYTER_TOKEN=<コピーした文字列> に変更

# 3. イメージをビルド
make build          # SBCL REPLのみ
make build-jupyter  # Jupyter Notebookのみ
make build-all      # 両方
```

### SBCL REPL環境

```shell
# 起動
make up

# 接続
make repl

# サンプル実行
make run-example

# 停止
make down
```

### Jupyter Notebook環境

```shell
# 起動
make up-jupyter

# ブラウザで開く
make notebook

# 停止
make down-jupyter
```

### よく使うコマンド

```shell
# ヘルプを表示
make help

# 状態確認
make status

# ログを確認
make logs          # REPL
make logs-jupyter  # Jupyter

# すべて停止
make down-all
```

---

## Install

### Docker環境（推奨）

#### 1. 環境変数の設定

```shell
# .env.exampleをコピー
make init

# または手動で
cp .env.example .env

# 必要に応じて.envを編集
# - SBCL_VERSION: SBCLバージョン（Alpine版のタグ）
# - COMPOSE_PROJECT_NAME: プロジェクト名
# - CONTAINER_NAME: コンテナ名
# - MEMORY_LIMIT: メモリ制限
# - CPU_LIMIT: CPU制限
# - USER_ID: ユーザーID（ホストと合わせる場合）
# - JUPYTER_PORT: Jupyter Notebookのポート
```

#### 2. コンテナの起動

**Makefileを使う（推奨）:**

```shell
# すべてのサービスを起動
make up

# SBCL REPLのみ起動
make up-sbcl

# Jupyter Notebookのみ起動
make up-jupyter
```

**docker composeコマンドを直接使う:**

SBCL REPL環境:
```shell
# 起動
docker compose up -d

# REPL接続
docker compose exec sbcl sbcl

# ファイル実行
docker compose exec sbcl sbcl --load src/hello.lisp

# 停止
docker compose down
```

Jupyter Notebook環境:
```shell
# 起動
docker compose -f docker-compose-jupyter.yml up -d

# ブラウザで http://localhost:8888 を開く

# 停止
docker compose -f docker-compose-jupyter.yml down
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

## Jupyter Notebook環境

### 🔐 セキュリティ設定（重要）

**⚠️ 警告: Jupyter Notebookはインターネットからアクセス可能な場合、強力な認証が必須です**

#### トークンの設定（必須）

`.env`ファイルで`JUPYTER_TOKEN`を設定してください:

```shell
# ✅ 推奨: 強力なランダムトークンを生成（必須）
openssl rand -hex 32

# 出力された文字列を .env の JUPYTER_TOKEN に設定
JUPYTER_TOKEN=<生成された64文字の文字列>
```

**セキュリティ要件:**
- ✅ **必須**: `openssl rand -hex 32`で生成した64文字のトークン
- ❌ **禁止**: 短い・予測可能なトークン（例: `my-token-2026`）
- ❌ **禁止**: 空のトークン（認証なし）
- ❌ **禁止**: `.env.example`のデフォルト値をそのまま使用

**リスク:**
- 弱いトークン → 不正アクセス、コード実行、データ漏洩
- トークンなし → 誰でもアクセス可能

**ネットワークセキュリティ:**
- ✅ デフォルトで`127.0.0.1`（localhost）にのみバインド
- ✅ 外部ネットワークからアクセス不可
- ⚠️ `.env`で`JUPYTER_HOST=0.0.0.0`に変更すると全インターフェースに公開（非推奨）

### 起動方法

**Makefileを使う（推奨）:**

```shell
# Jupyter Notebookを起動
make up-jupyter

# 起動時に以下の情報が表示されます:
# ==========================================
# 📍 アクセスURL:
#    http://localhost:8888/?token=your-token-here
#
# 🔑 トークン: your-token-here
# 🖥️  ホスト: localhost
# 🔌 ポート: 8888
# ==========================================

# ブラウザでアクセス（トークン付きURLで自動的に開きます）
make notebook
```

**docker composeコマンドを直接使う:**

```shell
# Jupyter Notebookを起動
docker compose -f docker-compose-jupyter.yml up -d

# .envファイルからトークンを確認
grep JUPYTER_TOKEN .env

# トークン付きURLでブラウザを開く
open "http://localhost:8888/?token=your-token-here"
```

### 使い方

1. `make up-jupyter` で起動（URLとトークンが表示されます）
2. `make notebook` でブラウザを開く（自動的にトークン付きURLで開きます）
3. `src/notebooks/` ディレクトリにノートブックを作成
4. カーネルで「Common Lisp」を選択
5. Lispコードを実行

### ノートブックの管理

- ✅ ノートブックは `src/notebooks/` に保存
- ✅ `.ipynb_checkpoints` は自動的に除外
- ✅ Docker内で完結（ホスト環境を汚染しない）

### 停止方法

**Makefileを使う:**

```shell
# Jupyter Notebookを停止
make stop-jupyter

# 完全に削除
make down-jupyter
```

**docker composeコマンドを直接使う:**

```shell
# Jupyter Notebookを停止
docker compose -f docker-compose-jupyter.yml stop

# 完全に削除
docker compose -f docker-compose-jupyter.yml down
```

## 言語仕様

