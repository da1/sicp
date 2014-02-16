;; 問題3.71
(load "./ch3/exercise3-70.scm")

(define (cube x) (* x x x))
(define (cube-weight x)
  (let ((i (car x))
        (j (cadr x)))
    (+ (cube i) (cube j))))
(define cube-weighted-stream
  (weighted-pairs integers integers cube-weight))

(define (ramanujan-sub s)
  (let ((s-weight (cube-weight (stream-car s)))
        (t-weight (cube-weight (stream-car (stream-cdr s)))))
    (if (= s-weight t-weight)
      (cons-stream s-weight
                   (ramanujan-sub (stream-cdr s)))
      (ramanujan-sub (stream-cdr s)))))

(define ramanujan-stream
  (ramanujan-sub cube-weighted-stream))

(show-stream ramanujan-stream 0 5)

