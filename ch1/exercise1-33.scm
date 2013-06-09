;; 問題1-33
;; accumulateにfilterを取り入れる

(load "./utils.scm")

(define (filtered-accumulate term a next b combiner null-value f)
  (define (iter a result)
    (if (> a b)
      result
      (iter
        (next a)
        (if (f (term a))
          (combiner result (term a))
          result))))
  (iter a null-value))

(define (sum-prime a b)
  (define (next x) (+ x 1))
  (define (term x) x)
  (filtered-accumulate term a next b + 0 prime?))

(sum-prime 2 11)
(+ 2 3 5 7 11)

(define (gcd-product n)
  (define (next x) (+ x 1))
  (define (term x) x)
  (define (gcd-filter x)
    (= (gcd x n) 1))
  (filtered-accumulate term 1 next n * 1 gcd-filter))

(gcd-product 10)
(* 1 3 7 9)
