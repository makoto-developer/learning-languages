.PHONY: help init build build-jupyter build-all rebuild rebuild-jupyter rebuild-all
.PHONY: up up-sbcl up-jupyter down down-jupyter down-all stop stop-jupyter restart restart-jupyter
.PHONY: clean clean-volumes logs logs-sbcl logs-jupyter ps status
.PHONY: repl shell notebook run run-example test watch install version env-check info

# デフォルトターゲット
.DEFAULT_GOAL := help

##@ ヘルプ

help: ## ヘルプを表示
	@echo 'Common Lisp開発環境'
	@echo ''
	@echo '使用方法:'
	@echo '  make <target>'
	@echo ''
	@awk 'BEGIN {FS = ":.*##"; printf "利用可能なコマンド:\n"} /^[a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)

##@ セットアップ

init: ## 初期セットアップ（.envファイル作成）
	@if [ ! -f .env ]; then \
		echo ".envファイルを作成しています..."; \
		cp .env.example .env; \
		echo "✅ .envファイルを作成しました"; \
	else \
		echo "⚠️  .envファイルは既に存在します"; \
	fi

##@ 起動・停止

up: ## SBCL REPLを起動
	docker compose up -d
	@echo "✅ SBCL REPLを起動しました"
	@echo ""
	@echo "接続: make repl"

up-sbcl: ## SBCL REPLを起動（upと同じ）
	docker compose up -d
	@echo "✅ SBCL REPLを起動しました (make repl で接続)"

up-jupyter: ## Jupyter Notebookを起動
	@docker compose -f docker-compose-jupyter.yml up -d
	@echo ""
	@echo "✅ Jupyter Notebookを起動しました"
	@echo ""
	@echo "=========================================="
	@. ./.env 2>/dev/null || true; \
	HOST=$${JUPYTER_HOST:-127.0.0.1}; \
	PORT=$${JUPYTER_PORT:-8888}; \
	TOKEN=$${JUPYTER_TOKEN}; \
	echo "📍 アクセスURL:"; \
	if [ -n "$$TOKEN" ]; then \
		echo "   http://localhost:$$PORT/?token=$$TOKEN"; \
	else \
		echo "   http://localhost:$$PORT/"; \
		echo ""; \
		echo "⚠️  警告: トークン認証が無効です"; \
		echo "   セキュリティのため .env で JUPYTER_TOKEN を設定してください"; \
	fi; \
	echo ""; \
	echo "🔑 トークン: $${TOKEN:-<未設定>}"; \
	echo "🖥️  バインド: $$HOST (localhostのみ)"; \
	echo "🔌 ポート: $$PORT"
	@echo "=========================================="
	@echo ""
	@echo "ブラウザで開く: make notebook"

down: ## SBCL REPLを停止・削除
	docker compose down

down-jupyter: ## Jupyter Notebookを停止・削除
	docker compose -f docker-compose-jupyter.yml down

down-all: ## すべてのサービスを停止・削除
	docker compose down
	docker compose -f docker-compose-jupyter.yml down

stop: ## SBCL REPLを停止（削除しない）
	docker compose stop

stop-jupyter: ## Jupyter Notebookを停止（削除しない）
	docker compose -f docker-compose-jupyter.yml stop

restart: ## SBCL REPLを再起動
	docker compose restart

restart-jupyter: ## Jupyter Notebookを再起動
	docker compose -f docker-compose-jupyter.yml restart

##@ ビルド

build: ## SBCL REPLイメージをビルド
	docker compose build

build-jupyter: ## Jupyter Notebookイメージをビルド
	docker compose -f docker-compose-jupyter.yml build

build-all: ## すべてのイメージをビルド
	docker compose build
	docker compose -f docker-compose-jupyter.yml build

rebuild: ## SBCL REPLイメージを再ビルド（キャッシュなし）
	docker compose build --no-cache

rebuild-jupyter: ## Jupyter Notebookイメージを再ビルド（キャッシュなし）
	docker compose -f docker-compose-jupyter.yml build --no-cache

rebuild-all: ## すべてのイメージを再ビルド（キャッシュなし）
	docker compose build --no-cache
	docker compose -f docker-compose-jupyter.yml build --no-cache

##@ クリーンアップ

clean: ## すべて削除（コンテナ・ボリューム・イメージ）
	@echo "⚠️  すべてのコンテナ、ボリューム、イメージを削除します"
	@read -p "続行しますか? [y/N] " confirm && [ "$$confirm" = "y" ] || exit 1
	docker compose down -v --rmi local
	docker compose -f docker-compose-jupyter.yml down -v --rmi local
	@echo "✅ クリーンアップ完了"

clean-volumes: ## ボリュームのみ削除
	docker compose down -v
	docker compose -f docker-compose-jupyter.yml down -v

##@ ログ・状態確認

logs: ## SBCL REPLのログを表示
	docker compose logs -f

logs-sbcl: ## SBCL REPLのログを表示
	docker compose logs -f

logs-jupyter: ## Jupyter Notebookのログを表示
	docker compose -f docker-compose-jupyter.yml logs -f

ps: ## 実行中のコンテナを表示
	@echo "=== SBCL REPL ==="
	@docker compose ps
	@echo ""
	@echo "=== Jupyter Notebook ==="
	@docker compose -f docker-compose-jupyter.yml ps

status: ## サービスの状態を表示
	@echo "=== SBCL REPL ==="
	@docker compose ps
	@echo ""
	@echo "=== Jupyter Notebook ==="
	@docker compose -f docker-compose-jupyter.yml ps

##@ 接続・アクセス

repl: ## SBCL REPLに接続
	docker compose exec sbcl sbcl

shell: ## コンテナのシェルに接続
	docker compose exec sbcl sh

notebook: ## Jupyter Notebookをブラウザで開く
	@echo "Jupyter Notebookを開きます..."
	@. ./.env 2>/dev/null || true; \
	PORT=$${JUPYTER_PORT:-8888}; \
	TOKEN=$${JUPYTER_TOKEN}; \
	if [ -n "$$TOKEN" ]; then \
		URL="http://localhost:$$PORT/?token=$$TOKEN"; \
	else \
		URL="http://localhost:$$PORT/"; \
	fi; \
	echo "URL: $$URL"; \
	sleep 1; \
	open "$$URL" 2>/dev/null || xdg-open "$$URL" 2>/dev/null || echo "ブラウザで開いてください: $$URL"

##@ 実行・テスト

run: ## Lispファイルを実行（usage: make run FILE=src/hello.lisp）
	@if [ -z "$(FILE)" ]; then \
		echo "エラー: FILEを指定してください"; \
		echo "例: make run FILE=src/hello.lisp"; \
		exit 1; \
	fi
	docker compose exec sbcl sbcl --load $(FILE) --quit

run-example: ## サンプルファイルを実行
	@echo "=== 01-basics.lisp ==="
	@docker compose exec sbcl sbcl --load src/01-basics.lisp --eval "(run-basics)" --quit
	@echo ""
	@echo "=== 02-lists.lisp ==="
	@docker compose exec sbcl sbcl --load src/02-lists.lisp --eval "(run-lists)" --quit
	@echo ""
	@echo "=== 03-control.lisp ==="
	@docker compose exec sbcl sbcl --load src/03-control.lisp --eval "(run-control)" --quit

test: ## テストを実行
	@echo "テストを実行..."
	@if [ -d "test" ]; then \
		docker compose exec sbcl sbcl --load test/run-tests.lisp --quit; \
	else \
		echo "⚠️  testディレクトリが存在しません"; \
	fi

##@ 開発

watch: ## ログを監視
	docker compose logs -f

install: ## Quicklispでパッケージをインストール（usage: make install PKG=alexandria）
	@if [ -z "$(PKG)" ]; then \
		echo "エラー: PKGを指定してください"; \
		echo "例: make install PKG=alexandria"; \
		exit 1; \
	fi
	docker compose exec sbcl sbcl --eval "(ql:quickload :$(PKG))" --quit

version: ## SBCLバージョンを表示
	@docker compose exec sbcl sbcl --version 2>&1 | grep SBCL || echo "コンテナが起動していません"

##@ その他

env-check: ## 環境設定を確認
	@echo "=== 環境設定 ==="
	@if [ -f .env ]; then \
		echo "✅ .env ファイル: 存在"; \
		echo ""; \
		cat .env; \
	else \
		echo "❌ .env ファイル: 存在しない"; \
		echo ""; \
		echo "実行: make init"; \
	fi

info: ## システム情報を表示
	@echo "=== Docker情報 ==="
	@docker --version
	@docker compose version
	@echo ""
	@echo "=== イメージ ==="
	@docker images | grep -E "REPOSITORY|common-lisp|clfoundation" || echo "イメージなし"
	@echo ""
	@echo "=== ボリューム ==="
	@docker volume ls | grep -E "DRIVER|common-lisp" || echo "ボリュームなし"
