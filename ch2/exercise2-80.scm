;; 問題2.80
; 引数がゼロかどうかテストする汎用演算 =zero? を定義して，汎用算術演算パッケージに設定せよ
; この演算は通常の数，有理数及び複素数に対して働こうとするものとする

(load "./ch2/2-5_system_with_generic_operations.scm")

(define n1 (make-scheme-number 0))
(zero? n1)

(define ra1 (make-rational 0 1))
(zero? ra1)

(define ri1 (make-complex-from-real-imag 0 0))
(zero? ri1)

(define ma1 (make-complex-from-mag-ang 0 0))
(zero? ma1)

