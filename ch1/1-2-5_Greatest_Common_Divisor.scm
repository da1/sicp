;; 1.2.5 最大公約数
;; Euclidのアルゴリズム
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; 1.20

;; 1.2.6 例 素数性のテスト
(define (square n)
  (* n n))
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
(use srfi-27)
(define (random n)
  (random-integer (- n 1)))
(random 100)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else #f)))
(fast-prime? 13 1000)

;; 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;; 1.22
;; http://sicp.g.hatena.ne.jp/n-oohira/?word=*%5Bgauche%5D
(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
              (+ (* a 1000000) b)))

(define (timed-prime-test n)
;  (newline)
;  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f))
(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes idx count)
  (cond ((even? idx) (search-for-prime (+ idx 1) count))
    ((= count 0) #f)
    (else
     (begin
       ;(timed-prime-test idx)
       (if (timed-prime-test idx)
           (search-for-prime (+ idx 2) (- count 1))
           (search-for-prime (+ idx 2) count))))))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)

;; 1.23
(define (square n)
  (* n n))
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
(prime? 7)
(prime? 201)

(define (timed-prime-test n)
;  (newline)
;  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f))
(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

;; gosh> find-divisor
;; gosh> 
;; 1009 *** 237#<undef>
;; gosh> 
;; 1013 *** 234#<undef>
;; gosh> 
;; 1019 *** 250#<undef>
;; gosh> 
;; 10007 *** 714#<undef>
;; gosh> 
;; 10009 *** 8872#<undef>
;; gosh> 
;; 10037 *** 853#<undef>
;; gosh> 
;; 100003 *** 3339#<undef>
;; gosh> 
;; 100019 *** 3806#<undef>
;; gosh> 
;; 100043 *** 5167#<undef>
;; gosh> 
;; 1000003 *** 9110#<undef>
;; gosh> 
;; 1000033 *** 12001#<undef>
;; gosh> 
;; 1000037 *** 7620#<undef>

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)

;; gosh> find-divisor
;; gosh> 
;; 1009 *** 161#<undef>
;; gosh> 
;; 1013 *** 240#<undef>
;; gosh> 
;; 1019 *** 227#<undef>
;; gosh> 
;; 10007 *** 487#<undef>
;; gosh> 
;; 10009 *** 481#<undef>
;; gosh> 
;; 10037 *** 483#<undef>
;; gosh> 
;; 100003 *** 1783#<undef>
;; gosh> 
;; 100019 *** 1722#<undef>
;; gosh> 
;; 100043 *** 1723#<undef>
;; gosh> 
;; 1000003 *** 8766#<undef>
;; gosh> 
;; 1000033 *** 5025#<undef>
;; gosh> 
;; 1000037 *** 2833#<undef>
;; gosh> 
