;; 問題1-32
;; sumやproductの更に一般化したaccumulate
(load "./ch1/1-3_Formulating_Abstractings_with_Higher-Order_Procedures.scm")

(define (accumulate term a next b combiner null-value)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate term (next a) next b combiner null-value))))

(define (sum term a next b)
  (accumulate term a next b + 0))

(define (product term a next b)
  (accumulate term a next b * 1))

(sum-cubes 1 10)
(sum-integers 1 10)

;; 反復プロセス版
(define (accumulate term a next b combiner null-value)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))
