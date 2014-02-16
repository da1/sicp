;; 問題3.76
(load "./ch3/exercise3-74.scm")

(define (smooth st)
  (stream-map (lambda (x y) (/ (+ x y) 2.0)) st (stream-cdr st)))

(define zero-crossings (make-zero-crossings (smooth sense-data) 0))
(show-stream zero-crossings 0 10)

