(define true #t)
(define false #f)

;1.1.4
(define (square x) (* x x))
;1.1.6
(define (abs x)
  (cond ((< x 0) (- x)) (else x)))

;; exercise1-7
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

;; 1.22
;; http://sicp.g.hatena.ne.jp/n-oohira/?word=*%5Bgauche%5D
(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
              (+ (* a 1000000) b)))

;; 1.2.5 最大公約数を求める
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

;; 素数判定
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

;; 2.2.1
(define nil '())

;; 問題3.5
;; 乱数
;; http://practical-scheme.net/wiliki/schemexref.cgi?SRFI-27
;; http://sicp.g.hatena.ne.jp/hyuki/20060505/random
(use srfi-27)



;; 4.1.1
;; schemeのapply手続きを退避
(define apply-in-underlying-scheme apply)

;; 3.5.1
(define-macro (delay x) `(memo-proc (lambda () ,x)))
; (define-macro (delay x) `(lambda () ,x))
(define (force x) (x))

;; 5
(define user-initial-environment interaction-environment)
