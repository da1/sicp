;; 問題1.44
;; 関数の平滑化

(define dx 0.0001)

(define (smooth f)
  (define (average x y z)
    (/ (+ x y z) 3))
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

((smooth sin) 0)

(define (n-smooth f n)
  (repeated (smooth f) n))

((n-smooth sin 2) 0)
