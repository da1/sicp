;; 問題1.41
(define (inc x) (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

((double inc) 0)

(((double (double double)) inc) 5)
