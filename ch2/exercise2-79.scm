;; 問題2.79
; 2つの数の等価をテストする等価述語equ?を定義して，汎用算術パッケージに設定せよ
; この演算は通常の数，有理数および複素数に対して働くものとする

(load "./ch2/2-5_system_with_generic_operations.scm")

;;2.79
(define n1 (make-scheme-number 3))
(define n2 (make-scheme-number 3))
(define n3 (make-scheme-number 4))
(equ? n1 n2)
(equ? n1 n3)

(define ra1 (make-rational 3 4))
(define ra2 (make-rational 3 4))
(define ra3 (make-rational 1 5))
(equ? ra1 ra2)
(equ? ra1 ra3)

(define ri1 (make-complex-from-real-imag 1 1))
(define ri2 (make-complex-from-real-imag 1 1))
(define ri3 (make-complex-from-real-imag 2 1))
(equ? ri1 ri2)
(equ? ri1 ri3)

(define ma1 (make-complex-from-mag-ang 1 0))
(define ma2 (make-complex-from-mag-ang 1 0))
(define ma3 (make-complex-from-mag-ang 1 1))
(equ? ma1 ma2)
(equ? ma1 ma3)

(define ri4 (make-complex-from-real-imag 1 0))
(equ? ri4 ma1)

