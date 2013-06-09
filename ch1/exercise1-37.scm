;; 問題1.37

;;a. 無限の連分数
;f = N1/(D1+N2/(D2+N3/(D3+... )

;; k項有限連分数

(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))
        ))
  (iter 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           1000)

(/ 1 (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100))

(define (cont-frac n d k)
  (define (iter result i)
    (if (= i 1)
      (/ (n i) (+ (d i) result))
      (iter (/ (n i) (+ (d i) result)) (- i 1))))
  (iter 0 k))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)
