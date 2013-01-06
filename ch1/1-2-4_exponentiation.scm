;; 1.2.4 べき乗
(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(define (square n)
  (* n n))
(define (fast-expt b n)
  (cond ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))))
    (else (* b (fast-expt b (- n 1))))))
(fast-expt 3 3)
(fast-expt 3 1000)

;; 1.16
(define (fast-expt2 b n)
  (fast-expt-iter b n 1))
(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
    ((even? n) (fast-expt-iter (* b b) (/ n 2) a))
    (else (fast-expt-iter b (- n 1) (* a b)))))
(fast-expt2 3 3)
(= (fast-expt 3 1000) (fast-expt2 3 1000))

;; 1.17
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
(* 3 4)
(define (double n)
  (+ n n))
(define (halve n)
  (if (even? n)
      (/ n 2)
      (error "not even" n)))
(double 3)
(halve 10)
(halve 5)

(define (* a b)
  (cond ((= b 0) 0)
    ((even? b) (double (* a (halve b))))
    (else (+ a (* a (- b 1))))))
(* 3 4)
(* 12 56)

;; 1.18

;; 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
    ((even? count)
     (fib-iter a
           b
           (+ (* p p) (* q q))
           (* q (+ q (* 2 p)))
           (/ count 2)))
    (else (fib-iter (+ (* b q) (* a q) (* a p))
            (+ (* b p) (* a q))
            p
            q
            (- count 1)))))
(fib 10)
