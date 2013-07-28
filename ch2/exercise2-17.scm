;; 問題2.17
;; 与えられたリストの最後の要素だけからなるリストを返す手続きを定義せよ

(define (last-pair a)
  (if (null? (cdr a))
    a
    (last-pair (cdr a))))

(last-pair (list 23 72 149 34))
