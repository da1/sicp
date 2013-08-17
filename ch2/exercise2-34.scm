;; 問題2.34
; Hornerの方法を使って，多項式の評価をする

(load "./ch2/2-2-3_Sequences_as_Conventional_Interfaces.scm")

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))
