;;; 01-basics.lisp - Common Lispの基本文法

;;; ============================================
;;; 1. コメント
;;; ============================================

;; これは一行コメント

#|
これは
複数行
コメント
|#


;;; ============================================
;;; 2. 基本的なデータ型
;;; ============================================

(defun show-data-types ()
  "基本的なデータ型の例"
  (format t "=== データ型 ===~%")

  ;; 数値
  (format t "整数: ~A~%" 42)
  (format t "浮動小数点: ~A~%" 3.14)
  (format t "分数: ~A~%" 1/3)

  ;; 文字列
  (format t "文字列: ~A~%" "Hello, Lisp!")

  ;; シンボル
  (format t "シンボル: ~A~%" 'my-symbol)

  ;; 真偽値
  (format t "真: ~A~%" t)
  (format t "偽: ~A~%" nil)

  ;; リスト
  (format t "リスト: ~A~%" '(1 2 3 4 5))
  (format t "空リスト: ~A~%" '())
  (terpri))


;;; ============================================
;;; 3. 基本的な算術演算
;;; ============================================

(defun arithmetic-examples ()
  "算術演算の例"
  (format t "=== 算術演算 ===~%")

  ;; 前置記法（プレフィックス記法）
  (format t "加算: (+ 1 2 3) = ~A~%" (+ 1 2 3))
  (format t "減算: (- 10 3) = ~A~%" (- 10 3))
  (format t "乗算: (* 4 5) = ~A~%" (* 4 5))
  (format t "除算: (/ 10 2) = ~A~%" (/ 10 2))
  (format t "剰余: (mod 10 3) = ~A~%" (mod 10 3))

  ;; ネスト
  (format t "ネスト: (* (+ 2 3) (- 10 5)) = ~A~%" (* (+ 2 3) (- 10 5)))
  (terpri))


;;; ============================================
;;; 4. 変数
;;; ============================================

;; グローバル変数（定数）
(defparameter *global-var* 100
  "グローバル変数の例")

(defconstant +pi+ 3.14159
  "定数の例（慣例として+で囲む）")

(defun variable-examples ()
  "変数の例"
  (format t "=== 変数 ===~%")

  ;; グローバル変数
  (format t "グローバル変数: *global-var* = ~A~%" *global-var*)
  (format t "定数: +pi+ = ~A~%" +pi+)

  ;; ローカル変数
  (let ((x 10)
        (y 20))
    (format t "ローカル変数: x = ~A, y = ~A~%" x y)
    (format t "合計: x + y = ~A~%" (+ x y)))

  (terpri))


;;; ============================================
;;; 5. 関数定義
;;; ============================================

;; 単純な関数
(defun square (x)
  "xの二乗を返す"
  (* x x))

;; 複数の引数
(defun add (a b)
  "2つの数を足す"
  (+ a b))

;; オプション引数
(defun greet (&optional (name "World"))
  "挨拶する（名前はオプション）"
  (format nil "Hello, ~A!" name))

;; キーワード引数
(defun make-person (&key name age city)
  "キーワード引数で人物情報を作成"
  (format nil "Name: ~A, Age: ~A, City: ~A" name age city))

(defun function-examples ()
  "関数の例"
  (format t "=== 関数 ===~%")

  (format t "square(5) = ~A~%" (square 5))
  (format t "add(3, 7) = ~A~%" (add 3 7))
  (format t "greet() = ~A~%" (greet))
  (format t "greet(\"Lisp\") = ~A~%" (greet "Lisp"))
  (format t "make-person: ~A~%" (make-person :name "Alice" :age 30 :city "Tokyo"))

  (terpri))


;;; ============================================
;;; 6. 比較演算
;;; ============================================

(defun comparison-examples ()
  "比較演算の例"
  (format t "=== 比較演算 ===~%")

  (format t "(= 5 5) = ~A~%" (= 5 5))
  (format t "(< 3 5) = ~A~%" (< 3 5))
  (format t "(> 3 5) = ~A~%" (> 3 5))
  (format t "(<= 5 5) = ~A~%" (<= 5 5))
  (format t "(>= 5 5) = ~A~%" (>= 5 5))

  ;; 文字列比較
  (format t "(string= \"hello\" \"hello\") = ~A~%" (string= "hello" "hello"))
  (format t "(string< \"abc\" \"xyz\") = ~A~%" (string< "abc" "xyz"))

  (terpri))


;;; ============================================
;;; メイン実行
;;; ============================================

(defun run-basics ()
  "すべての例を実行"
  (format t "~%========================================~%")
  (format t "Common Lisp 基本文法~%")
  (format t "========================================~%~%")

  (show-data-types)
  (arithmetic-examples)
  (variable-examples)
  (function-examples)
  (comparison-examples)

  (format t "完了！~%~%"))

;; ファイルをロードしたときに自動実行
;; コメントアウトを外すと自動実行されます
;; (run-basics)
