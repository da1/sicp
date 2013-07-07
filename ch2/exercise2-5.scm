;; 問題2.5
(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (cons-iter z n const)
  (if (= (remainder z const) 0)
    (cons-iter (quotient z const) (+ n 1) const)
    n))

(define (car z)
  (cons-iter z 0 2))

(define (cdr z)
  (cons-iter z 0 3))

(car (cons 2 3))
(cdr (cons 2 3))
