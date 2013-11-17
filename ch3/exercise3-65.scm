;; 問題3.65
(define (log-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (log-summands (+ n 1)))))

(define log-stream
  (partial-sums (log-summands 1)))

(show-stream log-stream 0 10)
(show-stream (euler-transform log-stream) 0 10)
(show-stream (accelerate-sequence euler-transform log-stream) 0 10)

