;;問題3.1
(define (make-accumulator sum)
  (lambda (x)
    (begin (set! sum (+ sum x))
           sum)))

(define A (make-accumulator 5))
(A 10)
;15
(A 10)
;25
