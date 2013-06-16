;; 問題1.42
(load "./utils.scm")
(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6)
