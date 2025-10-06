# Pattern match

`=`はElixirではマッチ演算子と呼ばれる。代入ではない。`x=1`でxを1に束縛したあと、`x`と`2`を束縛しようとするとエラーになる。

また、変数の束縛は1度しかできない(`[b, b] = [1, 2]`みたいなのはできない)。

Elixirで(というか関数型プログラミング全般で)おいて、`=`は今までのイコールとは意味を忘れなければならない。

```elixir
iex(4)> x = x
2
iex(5)> x = 1
1
iex(6)> 2 = x
** (MatchError) no match of right hand side value: 1
    (stdlib 4.3) erl_eval.erl:496: :erl_eval.expr/6
    iex:6: (file)

```

パターンマッチはいろいろなやり方がある。↓はリストの一部を取り出す。

```elixir
iex(6)> list = [ 1, 2, 3 ]
[1, 2, 3]
iex(7)> [ a, b, c ] = list
[1, 2, 3]
iex(8)> a
1
iex(9)> b
2
iex(10)> c
3
```

`_`を使って値を無視する。

```elixir
iex(12)> [t, _, _] = list
[1, 2, 3]
iex(13)> t
1
```

パターンマッチで変数を使いたい場合は`^`を使う(これはPhoenix/EctoでSelect文に関数の引数を埋め込む時とかに使ったりするので覚えておく)

```elixir
iex(14)> y = 9
9
iex(15)> [w, 9, 1] = [10, y, 1]
[10, 9, 1]
iex(16)> w
10
```

# 変数

Elixirでは基本的に変数は全て不変である。不変とは変数の値を書き換えることができない、変わることがないということが保証されている。

非効率な気もするが、従来のプログラミングの書き方の方が実は非効率だ。何か書き換えたり追加したくなったら、直接データを書き換えず、データを別の新しいデータを作る。

つまり、

```bash
a = [ 1, 2, 3 ]
a = [ 1, 2, 3, 4]
```

ではなく、次のように書く。

```elixir
a = [ 1, 2, 3 ]
b = [ a | 4 ]
```

こうすることで、元々の`a`を更新する処理(メモリを書き換える作業)を省き、`a`のメモリ領域と新たに`4`をメモリ確保した領域を繋げるだけで新しい変数bが生まれる。

つまり、既存の変数の全てもしくは一部を再利用して新たな変数を作る。

ElixirではListは連結リストなので追加する場合は処理コストを低くできる。

変換を行う関数は、その新しいコピーを返すだけなのでとてもシンプルになる。

また、変数はレキシカルスコープである。つまり、スコープの単位はモジュールまたは関数となる。トップレベルの変数は下位関数はアクセスが可能。

# 型

整数

```elixir
# 10進数, 8進数, 2進数, 16進数
iex(23)> 1
1
iex(24)> 19
19
iex(25)> 0o10
8
iex(26)> 0b111
7
iex(27)> 0xA  
10
```

浮動小数点数

```elixir
iex(31)> 1.0
1.0
iex(32)> 0.2345678
0.2345678
iex(33)> 18/2114119
8.514184868496049e-6
```

アトム

アトムは定数(もしくはタグのようなもの)と考えてよい。よく関数の戻り値とセットで`:ok`が返ってくることが多い。

例えば、`Date.new()`関数は結果と日付をセットで返す。

アトムはその名前自身が値になる。

```elixir
iex(34)> :ok
:ok
iex(35)> {:ok, date} = Date.new(2020, 12,12)
{:ok, ~D[2020-12-12]}
```

範囲

```elixir
iex(40)> 1..10
1..10
```

タプル

順序をもったコレクション。基本的に変更することができない。パターンマッチもできる。

```elixir
iex(41) { 1, 2 }
{ 1, 2 }
iex(43)> {status, result} = {:ok, 1}
{:ok, 1}
```

リスト

連結リストで実装されている。すなわち、追加は早いが全捜査が遅い。

```elixir
iex(44)> [ 1, 2, 3 ]
[ 1, 2, 3 ]
iex(44)> [ 1, 2 ] ++ [ 3, 4 ]
[1, 2, 3, 4]
iex(45)> [ 1, 2, 3, 4 ] -- [ 4 ]
[1, 2, 3]
iex(46)> 1 in [ 1, 2, 3 ]
true
```

キーワードリスト

実態はタプルのリストである。ショートカットした書き方が可能。

リストが期待される値で最後の要素がキーワードリストであればさらなる省略が可能

```elixir
iex(47)> [ name: "Bob", city: "Tokyo" ] # ショートカットした書き方
[name: "Bob", city: "Tokyo"]
iex(48)> [ { :name, "Bob" }, { :city, "Tokyo" } ] # 実際の書き方。どちらも実態は同じ。
[name: "Bob", city: "Tokyo"]
iex(49)> insert record, transaction: true, auto_commit: true, log_level: true
```

マップ

キーと値のペア。一般的なプログラミング言語と同じ性質をもつ。

キーは文字列でもよいが、アトムであれば.で呼び出すことができる。

```elixir
iex(50)> class_member = %{"bob" => [full_name: "bob smith"], "alice"=> [full_name: "alice eppse"]}
%{"alice" => [full_name: "alice eppse"], "bob" => [full_name: "bob smith"]}
iex(51)> class_member["bob"]
[full_name: "bob smith"]
iex(52)> status = %{:ok => 0, :error => 1}
%{error: 1, ok: 0}
iex(53)> status.ok
0
```

真偽値

true, false, nilを扱う。

```elixir
iex(55)> 1 == 1
true
iex(56)> 1 == 1.0
true
iex(57)> 1 !== 0
true
iex(58)> 1 > 1
false
iex(59)> 1 < 1
false
iex(60)> 1 <= 1
true
iex(61)> 1 >= 1
true
```

ブール演算子

or and not はnilを含まない値を比較できる。nilが予測される場合は`||`, `&&`, `!a`を使う。

```elixir
iex(63)> true or true
true
iex(64)> true and true
true
iex(65)> not false
true
iex(66)> nil || true
true
iex(67)> nil && true
nil
iex(68)> !nil
true

```

# 条件分岐

```elixir
if 5 - 3 = 2 do
  IO.inspect("ok")
end
```

# 無名関数

例えば、2つの引数を合計する関数は↓次のようになる。

.(ドット)は関数呼び出しの構文である。

```elixir
iex(85)> sum = fn a, b -> a + b end
#Function<41.3316493/2 in :erl_eval.expr/6>
iex(86)> sum.(1, 3)
4
```

# 関数

```elixir
handle_open = fn 
  {:ok, file} -> "First, line: #{IO.read(file, :line)}" 
  {_, error} -> "Error: #{:file.format_error(error)}"
end
```
