;;; Hello World サンプル

(defun hello ()
  "シンプルな挨拶関数"
  (format t "Hello, Common Lisp!~%"))

(defun greet (name)
  "名前を指定して挨拶"
  (format t "Hello, ~A!~%" name))

;; 実行例
;; (hello)
;; (greet "World")
