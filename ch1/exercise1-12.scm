;; 問題1.12
;; Pascal三角形

(define (pascal-triangle n k)
  (cond ((< n 3) 1)
    ((= k 1) 1)
    ((= k n) 1)
    (else (+ (pascal-triangle (- n 1) (- k 1))
         (pascal-triangle (- n 1) k)))))

(pascal-triangle 1 1)
(pascal-triangle 2 1)
(pascal-triangle 2 2)
(pascal-triangle 3 2)
(pascal-triangle 4 2)
(pascal-triangle 5 3)
