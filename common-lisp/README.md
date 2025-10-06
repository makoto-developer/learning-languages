# Learning Common-Lisp

## Install

```shell
brew install sbcl

```

## 言語仕様

まずは簡単に足し算

```shell
$ sbcl
This is SBCL 0.8.13.60, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (+ 2 2)

4
* (exit)
$
```

hello, worldから。

- アトム (atom) -> ()の中の要素をアトムという。
- リストはコンスセルを接続して構成される。
  - カー、クダーと呼ばれる格納する箱と結合する作用に相当する場所が存在する。
- formatが関数で、残りが引数(Tと"hello, world")
- 前置記法(ポーランド方式)で書くのが基本
- 

```shell
* (format t "hello, world")
hello, world
NIL
```

## Quicklisp

```shell
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
(quicklisp-quickstart:install)
```
