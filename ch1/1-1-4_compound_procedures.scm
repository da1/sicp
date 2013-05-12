;; 1.1.4 合成手続き
(define (square x) (* x x))

(square 21)
(square (+ 2 5))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(define (f a) 
  (sum-of-squares (+ a 1) (* a 2)))
(f 5)
