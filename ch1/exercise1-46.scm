;; 問題1.46
;; 反復改良法
(load "./utils.scm")

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
      guess
      (iter (improve guess))))
  iter)

(define (sqrt x)
  ((iterative-improve
     (lambda (guess)
       (< (abs (- (square guess) x))
          0.001))
     (lambda (guess)
       (average guess (/ x guess))))
   1.0))

(sqrt 4)

(define (fixed-point f first-guess)
  ((iterative-improve
     (lambda (guess) (< (abs (- (f guess) guess)) 0.00001))
     (lambda (guess) (f guess)))
   first-guess))

(fixed-point cos 1.0)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))
