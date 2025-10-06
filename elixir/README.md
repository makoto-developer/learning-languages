# Learning Elixir

## iex ヘルパー

- h -> ヘルパー関数の一覧が見れる
- c -> <code>c file.exs</code>でfile.exsをコンパイルする
- i -> 変数の詳細情報を確認できる(型情報など)
- flush -> メッセージを全て消去する
- r -> <code>r file.exs</code>でリコンパイルする
- 

## iexでコンパイル

```shell
$ iex                                                                                                                                               2023-05-07 10:00
Erlang/OTP 25 [erts-13.2] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:1] [jit:ns]

Interactive Elixir (1.14.3) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> pwd
/Users/user/work/github/learning_elixir
iex(2)> ls
.dockerignore     .git              .gitignore        .idea             
README.md         learning          
iex(3)> c "learning/spawn/pmap1.exs"
[Parallel]
iex(4)> Parallel.pmap 1..100, &(&1 * &1)
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100, 121, 144, 169, 196, 225, 256, 289, 324,
 361, 400, 441, 484, 529, 576, 625, 676, 729, 784, 841, 900, 961, 1024, 1089,
 1156, 1225, 1296, 1369, 1444, 1521, 1600, 1681, 1764, 1849, 1936, 2025, 2116,
 2209, 2304, 2401, 2500, ...]
iex(4)> 
```