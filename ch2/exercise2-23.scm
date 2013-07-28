;; 問題2.23
;; for-each
;; 引数として手続きと要素のリストを取る
;; 要素のそれぞれに左から右へ順に手続きを作用させる
;; 手続きを要素に作用させて返される値は使わない

(define (for-each proc items)
  (map proc items)
  #t)

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
