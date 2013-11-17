;;問題3.33
(define (averager a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (adder a b u)
    (multiplier v c u)
    (constant 2 v)
    'ok))

(define false #f)
(define true #t)

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))

(probe "A" A)
(probe "B" B)
(averager A B C)
(set-value! A 10 'user)
(set-value! B 30 'user)
(get-value C)
(forget-value! A 'user)
(set-value! A 90 'user)

