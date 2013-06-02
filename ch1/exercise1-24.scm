;; 問題1.24
(load "./ch1/1-2-6_Example_Testing_for_Primality.scm")

(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
      (report-prime n (- (runtime) start-time))
      #f))

(define (timed-prime-test n)
;  (print n)
  (start-prime-test n (runtime)))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes idx count)
  (cond ((even? idx) (search-for-primes (+ idx 1) count))
    ((= count 0) #f)
    (else
       ;(timed-prime-test idx)
       ;(print idx)
       (if (timed-prime-test idx)
           (search-for-primes (+ idx 2) (- count 1))
           (search-for-primes (+ idx 2) count)))))

(define (hoge lst)
  (cond ((null? lst) #f)
        (else
          (newline)
          (display "===== ")
          (display (car lst))
          (display " =====")
          (search-for-primes (car lst) 3)
          (hoge (cdr lst)))))

(hoge '(1000 10000 100000 1000000))
