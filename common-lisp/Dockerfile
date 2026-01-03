# Common Lisp開発環境 - セキュア設定（Alpine版）
FROM clfoundation/sbcl:2.5.10-alpine3.22

# ビルド引数
ARG USER_ID=1000

# セキュリティ: 非rootユーザーで実行
RUN adduser -D -u ${USER_ID} -s /bin/sh lispuser

# 作業ディレクトリ設定
WORKDIR /app

# Quicklispインストール（キャッシュ最適化）
COPY --chown=lispuser:lispuser quicklisp.lisp /tmp/quicklisp.lisp
RUN sbcl --non-interactive \
    --load /tmp/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install :path "/home/lispuser/quicklisp")' && \
    rm /tmp/quicklisp.lisp

# Quicklisp自動読み込み設定
RUN mkdir -p /home/lispuser && \
    echo '#-quicklisp' >> /home/lispuser/.sbclrc && \
    echo '(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))' >> /home/lispuser/.sbclrc && \
    echo '  (when (probe-file quicklisp-init)' >> /home/lispuser/.sbclrc && \
    echo '    (load quicklisp-init)))' >> /home/lispuser/.sbclrc && \
    chown lispuser:lispuser /home/lispuser/.sbclrc

# 依存関係インストール用ディレクトリ
RUN mkdir -p /app/src && chown -R lispuser:lispuser /app

# 非rootユーザーに切り替え
USER lispuser

# デフォルトコマンド: REPL起動
CMD ["sbcl"]
