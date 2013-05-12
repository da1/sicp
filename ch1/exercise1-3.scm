;; 問題1.3
;; 3つの引数をとり，大きい２つの数の二乗の和を返す手続きを定義せよ
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
