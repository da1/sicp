;; 問題2.70

(load "./ch2/exercise2-68.scm")
(load "./ch2/exercise2-69.scm")

(define pairs '((NA 16) (YIP 9) (SHA 3) (A 2) (GET 2) (JOB 2) (BOOM 1) (WAH 1)))

(define tree (generate-huffman-tree pairs))

(define message '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(encode message tree)

; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)

(length (encode message tree))

; 84bit
; 固定長方式を使っていた場合，符号を表すのに3bit必要
; 36 * 3 = 108
; 108bit必要

(length message)

