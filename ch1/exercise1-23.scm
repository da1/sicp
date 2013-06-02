;; 問題 1.23
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
;
