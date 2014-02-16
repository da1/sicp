;; 問題3.72
(load "./ch3/3-5-3_Exploiting_the_Stream_Paradigm.scm")

(define (square-weight x)
  (let ((i (car x))
        (j (cadr x)))
    (+ (square i) (square j))))
(define square-weighted-stream
  (weighted-pairs integers integers square-weight))
(define (sum-of-two-square-sub s)
  (let ((s-weight (square-weight (stream-car s)))
        (t-weight (square-weight (stream-car (stream-cdr s))))
        (u-weight (square-weight (stream-car (stream-cdr (stream-cdr s))))))
    (if (= s-weight t-weight u-weight)
      (cons-stream s-weight
                   (sum-of-two-square-sub (stream-cdr s)))
      (sum-of-two-square-sub (stream-cdr s)))))
(define sum-of-two-square-three-way
  (sum-of-two-square-sub square-weighted-stream))

(show-stream sum-of-two-square-three-way 0 5)

