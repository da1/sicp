(define (cube-iter old new x)
  (if (good-enough? old new)
      new
      (cube-iter new (improve new x) x)))

; oldとnewで値がほとんど変わらない(0.0001)ようならTrue
(define (good-enough? old new)
  (< (abs (- (/ old new) 1.0)) 0.0001))

(define (improve guess x)
  (div3 (/ x (* guess guess)) (* 2.0 guess)))

(define (div3 x y)
  (/ (+ x y) 3.0))

(define (cube-root x)
  (cube-iter x 1.0 x))

(cube-root 8)
(cube-root 1000)