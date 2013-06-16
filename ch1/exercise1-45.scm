;; 問題1.45
(load "./ch1/1-3-4_Procedures_as_Returned_Values.scm")
(load "./ch1/exercise1-43.scm")

(define (n-th-sqrt x n c)
    (fixed-point
      ((repeated average-damp c)
       (lambda (y) (/ x (expt y (- n 1)))))
      1.0))

(n-th-sqrt (expt 3 3) 3 1)
(n-th-sqrt (expt 3 4) 4 2)
(n-th-sqrt (expt 3 5) 5 2)
(n-th-sqrt (expt 3 6) 6 2)
(n-th-sqrt (expt 3 7) 7 2)
(n-th-sqrt (expt 3 8) 8 3)

(n-th-sqrt (expt 3 16) 16 4)

; log(n) 回の平均緩和が必要と思われる

