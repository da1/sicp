;; 1.2.2
(define (fib n)
  (cond ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
    ((or (< amount 0) (= kinds-of-coins 0)) 0)
    (else (+ (cc amount (- kinds-of-coins 1))
         (cc (- amount (first-denomination kinds-of-coins))
             kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
    ((= kinds-of-coins 2) 5)
    ((= kinds-of-coins 3) 10)
    ((= kinds-of-coins 4) 25)
    ((= kinds-of-coins 5) 50)))

;; 1.11
;; f(n) = n ; n < 3
;; f(n) = f(n-1)+2f(n-2)+3f(n-3) n>=3
(define (f n)
  (cond ((< n 3) n)
    (else (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3)))))))
(f 1)
(f 2)
(f 3)
(f 4)

;; 1.12
(define (pascal-triangle n k)
  (cond ((< n 3) 1)
    ((= k 1) 1)
    ((= k n) 1)
    (else (+ (pascal-triangle (- n 1) (- k 1))
         (pascal-triangle (- n 1) k)))))

(pascal-triangle 1 1)
(pascal-triangle 2 1)
(pascal-triangle 2 2)
(pascal-triangle 3 2)
(pascal-triangle 4 2)
(pascal-triangle 5 3)

;; 1.13
