;; 問題2.87
;;汎用算術演算パッケージに多項式用のzero?を追加

(load "./ch2/2-5-3_example_symbolic_algebra.scm")
(define c (make-polynomial 'x '((0 0))))
c
(=zero? a)
(=zero? c)
