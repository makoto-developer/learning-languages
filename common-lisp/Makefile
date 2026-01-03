.PHONY: help build up down start stop restart repl run clean logs

# デフォルトターゲット
help:
	@echo "Common Lisp開発環境 - 利用可能なコマンド:"
	@echo ""
	@echo "  make build    - Dockerイメージをビルド"
	@echo "  make up       - コンテナを起動（デタッチモード）"
	@echo "  make down     - コンテナを停止して削除"
	@echo "  make start    - 停止中のコンテナを起動"
	@echo "  make stop     - コンテナを停止"
	@echo "  make restart  - コンテナを再起動"
	@echo "  make repl     - SBCL REPLを起動"
	@echo "  make run FILE=src/hello.lisp - 指定したLispファイルを実行"
	@echo "  make logs     - コンテナのログを表示"
	@echo "  make clean    - コンテナとボリュームを削除"
	@echo ""

# Dockerイメージをビルド
build:
	docker compose build

# コンテナを起動（デタッチモード）
up:
	docker compose up -d

# コンテナを停止して削除
down:
	docker compose down

# 停止中のコンテナを起動
start:
	docker compose start

# コンテナを停止
stop:
	docker compose stop

# コンテナを再起動
restart:
	docker compose restart

# SBCL REPLを起動
repl:
	docker compose exec sbcl sbcl

# Lispファイルを実行
# 使い方: make run FILE=src/hello.lisp
run:
	@if [ -z "$(FILE)" ]; then \
		echo "エラー: FILEパラメータが必要です"; \
		echo "使い方: make run FILE=src/hello.lisp"; \
		exit 1; \
	fi
	docker compose exec sbcl sbcl --load $(FILE)

# コンテナのログを表示
logs:
	docker compose logs -f

# コンテナとボリュームを削除（クリーンアップ）
clean:
	docker compose down -v
