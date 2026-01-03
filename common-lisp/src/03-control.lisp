;;; 03-control.lisp - 制御構造

;;; ============================================
;;; 1. 条件分岐（IF）
;;; ============================================

(defun if-examples ()
  "IF文の例"
  (format t "=== IF文 ===~%")

  ;; 基本的なIF
  (let ((x 10))
    (format t "x = ~A~%" x)
    (format t "x > 5 ? ~A~%"
            (if (> x 5)
                "Yes"
                "No")))

  ;; THEN節のみ
  (let ((age 20))
    (if (>= age 20)
        (format t "成人です~%")))

  (terpri))


;;; ============================================
;;; 2. 複数条件（COND）
;;; ============================================

(defun cond-examples ()
  "COND文の例"
  (format t "=== COND文 ===~%")

  ;; 数値の判定
  (defun classify-number (n)
    (cond
      ((< n 0) "負の数")
      ((= n 0) "ゼロ")
      ((> n 0) "正の数")))

  (format t "10は: ~A~%" (classify-number 10))
  (format t "0は: ~A~%" (classify-number 0))
  (format t "-5は: ~A~%" (classify-number -5))

  ;; 成績判定
  (defun grade (score)
    (cond
      ((>= score 90) "A")
      ((>= score 80) "B")
      ((>= score 70) "C")
      ((>= score 60) "D")
      (t "F"))) ; t = デフォルトケース

  (format t "85点は: ~A~%" (grade 85))
  (format t "55点は: ~A~%" (grade 55))

  (terpri))


;;; ============================================
;;; 3. WHEN と UNLESS
;;; ============================================

(defun when-unless-examples ()
  "WHEN/UNLESSの例"
  (format t "=== WHEN/UNLESS ===~%")

  (let ((temperature 30))
    (format t "気温: ~A度~%" temperature)

    ;; WHEN: 条件が真の時だけ実行
    (when (> temperature 25)
      (format t "暑いです~%")
      (format t "水分補給してください~%"))

    ;; UNLESS: 条件が偽の時だけ実行
    (unless (< temperature 10)
      (format t "寒くはないです~%")))

  (terpri))


;;; ============================================
;;; 4. ループ（LOOP）
;;; ============================================

(defun loop-examples ()
  "LOOP文の例"
  (format t "=== LOOP文 ===~%")

  ;; 単純な繰り返し
  (format t "1から5まで:~%")
  (loop for i from 1 to 5
        do (format t "  ~A~%" i))

  ;; リストの走査
  (format t "リストの要素:~%")
  (loop for item in '(apple banana cherry)
        do (format t "  ~A~%" item))

  ;; 値を集める
  (format t "1から10の二乗: ~A~%"
          (loop for i from 1 to 10
                collect (* i i)))

  ;; 条件付き
  (format t "1から10の偶数: ~A~%"
          (loop for i from 1 to 10
                when (evenp i)
                collect i))

  ;; 合計
  (format t "1から100の合計: ~A~%"
          (loop for i from 1 to 100
                sum i))

  (terpri))


;;; ============================================
;;; 5. DO ループ
;;; ============================================

(defun do-examples ()
  "DO文の例"
  (format t "=== DO文 ===~%")

  ;; カウントダウン
  (format t "カウントダウン:~%")
  (do ((i 5 (- i 1)))
      ((< i 1) 'done)
    (format t "  ~A~%" i))

  ;; 複数の変数
  (format t "階乗計算:~%")
  (do ((n 1 (+ n 1))
       (factorial 1 (* factorial n)))
      ((> n 5) factorial)
    (format t "  ~A! = ~A~%" n factorial))

  (terpri))


;;; ============================================
;;; 6. DOTIMES と DOLIST
;;; ============================================

(defun dotimes-dolist-examples ()
  "DOTIMES/DOLISTの例"
  (format t "=== DOTIMES/DOLIST ===~%")

  ;; DOTIMES: 指定回数繰り返す
  (format t "DOTIMES (5回):~%")
  (dotimes (i 5)
    (format t "  繰り返し ~A~%" i))

  ;; DOLIST: リストの各要素に処理
  (format t "DOLIST:~%")
  (dolist (fruit '(apple banana cherry))
    (format t "  果物: ~A~%" fruit))

  (terpri))


;;; ============================================
;;; 7. 再帰
;;; ============================================

(defun factorial (n)
  "階乗を再帰で計算"
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defun fibonacci (n)
  "フィボナッチ数列を再帰で計算"
  (cond
    ((<= n 0) 0)
    ((= n 1) 1)
    (t (+ (fibonacci (- n 1))
          (fibonacci (- n 2))))))

(defun recursion-examples ()
  "再帰の例"
  (format t "=== 再帰 ===~%")

  (format t "5の階乗: ~A~%" (factorial 5))
  (format t "10の階乗: ~A~%" (factorial 10))

  (format t "フィボナッチ数列 (0-10):~%")
  (loop for i from 0 to 10
        do (format t "  F(~A) = ~A~%" i (fibonacci i)))

  (terpri))


;;; ============================================
;;; 8. CASE文（switch的な）
;;; ============================================

(defun case-examples ()
  "CASE文の例"
  (format t "=== CASE文 ===~%")

  (defun day-type (day)
    (case day
      ((monday tuesday wednesday thursday friday) '平日)
      ((saturday sunday) '休日)
      (otherwise '不明)))

  (format t "月曜日: ~A~%" (day-type 'monday))
  (format t "土曜日: ~A~%" (day-type 'saturday))
  (format t "holiday: ~A~%" (day-type 'holiday))

  (terpri))


;;; ============================================
;;; メイン実行
;;; ============================================

(defun run-control ()
  "すべての制御構造例を実行"
  (format t "~%========================================~%")
  (format t "Common Lisp 制御構造~%")
  (format t "========================================~%~%")

  (if-examples)
  (cond-examples)
  (when-unless-examples)
  (loop-examples)
  (do-examples)
  (dotimes-dolist-examples)
  (recursion-examples)
  (case-examples)

  (format t "完了！~%~%"))

;; 自動実行（コメントアウトを外す）
;; (run-control)
