;; 1.2.6 例 素数性のテスト
(load "./utils.scm")

(define (square n) (* n n))
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
(prime? 7)
(prime? 201)

;; Fermatテスト
;; Fermatの小定理
;; nを素数，aをnより小さい正の任意の整数とすると，aのn乗はnを法として，aと合同である．
(define (expmod base exp m)
  (cond ((= exp 0) 1)
    ((even? exp)
     (remainder (square (expmod base (/ exp 2) m))
            m))
    (else
     (remainder (* base (expmod base (- exp 1) m))
            m))))

(define (random n)
  (random-integer (- n 1)))
; (random 100)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else #f)))

; (fast-prime? 13 1000)

