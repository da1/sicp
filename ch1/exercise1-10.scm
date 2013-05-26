;アッカーマン関数
;wikipediaとは式の形がちょっと違う
;http://ja.wikipedia.org/wiki/%E3%82%A2%E3%83%83%E3%82%AB%E3%83%BC%E3%83%9E%E3%83%B3%E9%96%A2%E6%95%B0
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))
