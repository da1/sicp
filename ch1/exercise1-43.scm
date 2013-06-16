;; 問題1.43
(load "./ch1/exercise1-42.scm")

(define (repeated f n)
  (if (<= n 1)
    f
    (compose f (repeated f (- n 1)))))

((repeated inc 4) 0)

((repeated square 2) 5)
