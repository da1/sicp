;; 問題1.30
;; 反復的にsumを計算できるように書きなおす．

(load "./ch1/1-3_Formulating_Abstractings_with_Higher-Order_Procedures.scm")

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(sum cube 1 inc 10)
(sum identity 1 inc 10)
