;; 1.1.6 条件式と述語

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(abs 10) (abs 0) (abs -5)

(define (abs x)
  (cond ((< x 0) (- x)) (else x)))
(abs 10) (abs 0) (abs -5)

(define (abs x)
  (if (< x 0) (- x) x))
(abs 10) (abs 0) (abs -5)
