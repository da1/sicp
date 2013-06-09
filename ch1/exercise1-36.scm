;; 問題1.36
;; 生成する近似値を順に印字するようにfixed-pointを修正する

(load "./ch1/1-3-3_Procedures_as_General_Methods.scm")

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point (lambda (y)
               (/ (log 1000) (log y))) 2.0)

(fixed-point (lambda (y)
               (average y (/ (log 1000) (log y)))) 2.0)
