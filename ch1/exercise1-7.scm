;; 問題1.7
; good-enough? は非常に小さい数の平方根をとるときには効果的ではない

(define (sqrt-iter old new x)
  (if (good-enough? old new)
      new
      (sqrt-iter new (improve new x) x)))

; oldとnewで値がほとんど変わらない(0.001)ようならTrue
(define (good-enough? old new)
  (< (abs (- 1.0 (/ old new))) 0.001))

;元の二次式の接線のグラフの解
;元の二次式の解と近い値
(define (improve guess x)
  (average guess (/ x guess)))
;平均値
(define (average x y)
  (/ (+ x y) 2))

;平方根を求める関数
(define (sqrt x)
  (sqrt-iter x 1.0 x))

(sqrt 9)
(sqrt 1.0E-6)
(square (sqrt 1.0E26))

