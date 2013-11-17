;; 問題3.80
(define (RLC R L C dt)
  (define (rlc vC0 iL0)
    (define dvC (scale-stream iL (/ -1 C)))
    (define diL (add-streams (scale-stream vC (/ 1 L))
                             (scale-stream iL (- (/ R L)))))
    (define iL (integral (delay diL) iL0 dt))
    (define vC (integral (delay dvC) vC0 dt))
    ;;    (stream-map (lambda (v i) (cons v i)) vC iL))
    (stream-map cons vC iL))
  rlc)

(define RLC1 (RLC 1 1 0.2 0.1))
(show-stream (RLC1 10 0) 0 10)

