;; 問題2.94
(load "./ch2/exercise2-91.scm")

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

(define (greatest-common-divisor a b)
  (apply-generic 'greatest-common-divisor a b))

;; x^4 - x^3 - 2x^2 + 2x
(define p1 (make-polynomial 'x (make-sparse-term '((4 1) (3 -1) (2 -2) (1 2))))) 
 p1
; (polynomial x (4 1) (3 -1) (2 -2) (1 2))
;; x^3 - x
(define p2 (make-polynomial 'x (make-sparse-term '((3 1) (1 -1))))) 
p2
;(polynomial x (3 1) (1 -1))
(greatest-common-divisor p1 p2)
;gosh> (polynomial x (2 -1) (1 1))

