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

