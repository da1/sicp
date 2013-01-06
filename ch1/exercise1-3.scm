(define (three-sum-of-squares x y z)
  (cond ((and (<= x y) (<= x z)) (sum-of-squares y z))
	((and (<= y x) (<= y z)) (sum-of-squares x z))
	((and (<= z x) (<= z y)) (sum-of-squares x y))))

(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(three-sum-of-squares 3 4 5)
(three-sum-of-squares 4 5 5)
(three-sum-of-squares 4 4 5)