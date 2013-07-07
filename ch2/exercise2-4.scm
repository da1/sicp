;; 問題2.4
;; 対のもう一つの手続き表現

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

;; cdrの定義は何か
(define (cdr z)
  (z (lambda (p q) q)))

(car (cons 1 2))
(cdr (cons 1 2))
