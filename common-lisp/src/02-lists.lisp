;;; 02-lists.lisp - リスト操作（Lispの核心）

;;; ============================================
;;; 1. リストの基本
;;; ============================================

(defun list-basics ()
  "リストの基本操作"
  (format t "=== リストの基本 ===~%")

  ;; リストの作成
  (let ((list1 '(1 2 3 4 5))
        (list2 (list 10 20 30))
        (list3 '(a b c d)))

    (format t "list1: ~A~%" list1)
    (format t "list2: ~A~%" list2)
    (format t "list3: ~A~%" list3)

    ;; 空リスト
    (format t "空リスト: ~A~%" '())
    (format t "nilも空リスト: ~A~%" nil)
    (terpri)))


;;; ============================================
;;; 2. リストの基本操作（CAR, CDR, CONS）
;;; ============================================

(defun car-cdr-cons ()
  "CAR, CDR, CONSの例"
  (format t "=== CAR, CDR, CONS ===~%")

  (let ((my-list '(1 2 3 4 5)))

    ;; CAR: リストの最初の要素
    (format t "CAR (最初の要素): (car ~A) = ~A~%" my-list (car my-list))

    ;; CDR: 最初の要素以外
    (format t "CDR (残り): (cdr ~A) = ~A~%" my-list (cdr my-list))

    ;; CONS: リストの先頭に要素を追加
    (format t "CONS: (cons 0 ~A) = ~A~%" my-list (cons 0 my-list))

    ;; 組み合わせ
    (format t "CADR (2番目): (cadr ~A) = ~A~%" my-list (cadr my-list))
    (format t "CDDR (3番目以降): (cddr ~A) = ~A~%" my-list (cddr my-list))
    (terpri)))


;;; ============================================
;;; 3. よく使うリスト関数
;;; ============================================

(defun common-list-functions ()
  "よく使うリスト関数"
  (format t "=== よく使うリスト関数 ===~%")

  (let ((numbers '(1 2 3 4 5))
        (words '(hello world lisp programming)))

    ;; LENGTH: リストの長さ
    (format t "LENGTH: ~A の長さ = ~A~%" numbers (length numbers))

    ;; NTH: n番目の要素（0始まり）
    (format t "NTH: 2番目の要素 = ~A~%" (nth 2 numbers))

    ;; FIRST, SECOND, THIRD
    (format t "FIRST: ~A~%" (first numbers))
    (format t "SECOND: ~A~%" (second numbers))
    (format t "THIRD: ~A~%" (third numbers))

    ;; LAST: 最後の要素（リストとして）
    (format t "LAST: ~A~%" (last numbers))

    ;; REVERSE: 逆順
    (format t "REVERSE: ~A~%" (reverse numbers))

    ;; APPEND: リストの結合
    (format t "APPEND: ~A + ~A = ~A~%"
            numbers '(6 7 8)
            (append numbers '(6 7 8)))

    ;; MEMBER: 要素の検索
    (format t "MEMBER: 3は~Aに含まれる？ ~A~%"
            numbers
            (if (member 3 numbers) "Yes" "No"))

    (terpri)))


;;; ============================================
;;; 4. リストの変換
;;; ============================================

(defun list-transformations ()
  "リストの変換（MAP系関数）"
  (format t "=== リストの変換 ===~%")

  (let ((numbers '(1 2 3 4 5)))

    ;; MAPCAR: 各要素に関数を適用
    (format t "元のリスト: ~A~%" numbers)
    (format t "各要素を2倍: ~A~%" (mapcar (lambda (x) (* x 2)) numbers))
    (format t "各要素を二乗: ~A~%" (mapcar (lambda (x) (* x x)) numbers))

    ;; REMOVE-IF: 条件に合う要素を削除
    (format t "偶数を削除: ~A~%" (remove-if #'evenp numbers))
    (format t "奇数を削除: ~A~%" (remove-if #'oddp numbers))

    ;; REMOVE-IF-NOT (= FILTER)
    (format t "3より大きい要素のみ: ~A~%"
            (remove-if-not (lambda (x) (> x 3)) numbers))

    (terpri)))


;;; ============================================
;;; 5. リストの集計
;;; ============================================

(defun list-aggregations ()
  "リストの集計"
  (format t "=== リストの集計 ===~%")

  (let ((numbers '(1 2 3 4 5)))

    ;; REDUCE: 畳み込み
    (format t "合計: ~A~%" (reduce #'+ numbers))
    (format t "積: ~A~%" (reduce #'* numbers))
    (format t "最大値: ~A~%" (reduce #'max numbers))
    (format t "最小値: ~A~%" (reduce #'min numbers))

    ;; COUNT-IF: 条件に合う要素の数
    (format t "偶数の個数: ~A~%" (count-if #'evenp numbers))
    (format t "3より大きい個数: ~A~%"
            (count-if (lambda (x) (> x 3)) numbers))

    (terpri)))


;;; ============================================
;;; 6. ネストされたリスト
;;; ============================================

(defun nested-lists ()
  "ネストされたリスト"
  (format t "=== ネストされたリスト ===~%")

  (let ((matrix '((1 2 3)
                  (4 5 6)
                  (7 8 9))))

    (format t "行列: ~A~%" matrix)
    (format t "1行目: ~A~%" (first matrix))
    (format t "2行目: ~A~%" (second matrix))
    (format t "1行1列: ~A~%" (first (first matrix)))
    (format t "2行3列: ~A~%" (third (second matrix)))

    ;; FLATTEN的な操作
    (format t "平坦化: ~A~%" (reduce #'append matrix))

    (terpri)))


;;; ============================================
;;; メイン実行
;;; ============================================

(defun run-lists ()
  "すべてのリスト操作例を実行"
  (format t "~%========================================~%")
  (format t "Common Lisp リスト操作~%")
  (format t "========================================~%~%")

  (list-basics)
  (car-cdr-cons)
  (common-list-functions)
  (list-transformations)
  (list-aggregations)
  (nested-lists)

  (format t "完了！~%~%"))

;; 自動実行（コメントアウトを外す）
;; (run-lists)
