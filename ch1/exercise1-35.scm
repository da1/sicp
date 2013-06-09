;; 問題1.35
;; 1.2.2の黄金比Φ

(load "./ch1/1-3-3_Procedures_as_General_Methods.scm")

(define phi
  (fixed-point (lambda (y)
                 (average y (+ 1 (/ 1 y)))) 1.0))

phi
