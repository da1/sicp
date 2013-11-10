;問題3.6
(define rand
  (let ((x 11))
    (define (reset new-value)
      (set! x new-value) x)
    (define (generate)
      (set! x (rand-update x)) x)
    (define (dispatch m)
      (cond ((eq? m 'reset) reset)
            ((eq? m 'generate) (generate))
            (else (error "Unknown request -- RAND" m))))
    dispatch))

(rand 'generate)
((rand 'reset) 101)
((rand 'reset) 11)
