(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

;元の二次式の接線のグラフの解
;元の二次式の解と近い値
(define (improve guess x)
  (average guess (/ x guess)))
;平均値
(define (average x y)
  (/ (+ x y) 2))
;guessとxが十分に近い数字かチェック
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;平方根を求める関数
(define (sqrt x)
  (sqrt-iter 1.0 x))
