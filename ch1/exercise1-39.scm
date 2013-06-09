;; 問題1.39
;; 正接関数の連分数展開は1770年にドイツの数学者J.H.Lambertが発表した

(load "./ch1/exercise1-37.scm")

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                 x
                 (- (* x x))))
             (lambda (i)
               (- (* 2 i) 1))
             k))

(define pi 3.14159265359)
(tan-cf pi 100)
(tan pi)

(tan-cf (/ pi 4) 10)
(tan (/ pi 4))
