# Learning Rust

## 参考にした公式サイト

- https://doc.rust-lang.org/rust-by-example/hello/print/print_debug.html
- https://zenn.dev/khale/articles/rust-beginners-catchup
- https://doc.rust-jp.rs/book-ja/ch20-01-single-threaded.html

## なぜRust

プログラミング側面からの魅力
- リッチな型システム
- メモリ安全
  - メモリ破壊、未定義動作を未然に防ぐ
- 並行処理
  - オーバーヘッドが少ない非同期I/O

商用利用における魅力
- エコシステムが充実
- LinuxやCantrip(Googleが作ったセキュアOS、2022/10/25発表時はKataOSとあったが名称変更があった)など実績がある。
- Web/Native App/組み込み/OS...あらゆる領域に対応している

個人的
- とりあえずRustを使っておけば困らない感がある

## マスコット

Ferris(Rust界隈ではrustacean甲殻類と呼ぶ)

## 基礎文法

まずはHello Worldを、`vi hello.rx`でファイルを編集する
```rust

fn main() {
    println!("Hello World!");
}
```

rustcでコンパイル
```shell
$ rustc hello.rx
```

プロジェクトを作成

```shell
cargo new hello_cargo --bin
```

プロジェクトをビルド -> target/debugディレクトリが作成される

```shell
cargo build
```

プロジェクトを実行(必要な時は再ビルドされる)

```shell
cargo run
```

コンパイルチェック

```shell
cargo check
```

リリース -> target/releaseディレクトリが作成される

```shell
cargo build --release
cargo run --release
```

パッケージ管理
```shell
# 追加
cargo add rand
# 検索
cargo search
# buildを削除
cargo clean
# 追加したパッケージを削除
cargo uninstall
# lockファイルを作り直す
cargo update
```

### Cargo

パッケージマネージャーなどを含むあらゆるツールチェイン。JavaScriptのようにどれを選べばいいかなど迷わずにデフォルトで最初からすべて揃った状態でCargoを使えばみんな同じツールで開発ができる。

- rustup: バージョン選択ツール
- rustc: コンパイラ
- rustfmt: フォーマッタ
- rustdoc: ドキュメント作成
- clippy: lint
- racer: 補完
- rls: エディタと連携

## 学習方法

1. 公式ドキュメントを読む

```shell
rustup doc
```

2. TRPLを読む
日本語版 -> https://doc.rust-jp.rs/book-ja/


