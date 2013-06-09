;; 問題1.34
(load "./utils.scm")

(define (f g)
  (g 2))

(f square)
; 4
(f (lambda (z) (* z (+ z 1))))
; 6

(f f)
; (2 2)を評価しようとしてエラー
