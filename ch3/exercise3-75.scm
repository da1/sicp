;; 問題3.75
(load "./ch3/exercise3-74.scm")

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream) avpt))))

(define zero-crossings (make-zero-crossings sense-data 0 0))
(show-stream zero-crossings 0 10)

