;; 問題2.77
; Louis Reasoner はzをを評価しようとしたが動かなかった

(load "./ch2/2-5_system_with_generic_operations.scm")

(define z (make-complex-from-real-imag 3 4))
(magnitude z)
