;; 3.5.2 無限ストリーム
;; 無限の長さをもったストリームを扱う
(load "./ch3/exercise3-50.scm")

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(stream-car integers)
(stream-cdr integers)

;; 7で割り切れない整数のStream
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7))) integers))

(stream-ref no-sevens 100)

;; フィボナッチ数の無限ストリーム
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
;; (0, fib(1,1))
;; (0, (1, fib(1,2)))
;; (0, (1, (1, fib(2, 3))))
;; (0, (1, (1, (2, fib(3, 5)))))


;; no-sevensを一般化する
;; エラトステネスの篩 素数の無限ストリーム
(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 50)

;; ストリームの暗黙の定義
;; 無限ストリームを暗黙に定義する
;; 1の無限ストリーム
(define ones (cons-stream 1 ones))
;; ２つのストリームの和

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))
(stream-ref integers 10)

;; フィボナッチ数
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (show-stream stream n m)
  (if (< n m)
    (begin
      (display (stream-ref stream n))
      (newline)
      (show-stream stream (+ n 1) m))
    (stream-ref stream n)))
(show-stream fibs 0 5)

;; 2のべき乗のストリーム
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define double (cons-stream 1 (scale-stream double 2)))
(show-stream double 0 10)

;; 素数
(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes))
;; psのリストから順に素数を取り出していって，nが素数で割り切れたらfalse
;; 素数がroot nより大きくなったらおしまい．trueを返す．
(show-stream primes 0 10)

