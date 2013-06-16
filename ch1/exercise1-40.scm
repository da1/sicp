;; 問題1.40

(load "./ch1/1-3-4_Procedures_as_Returned_Values.scm")

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

; (newtons-method (cubic a b c) 1.0)
(newtons-method (cubic 1 0 4) 1.0)
