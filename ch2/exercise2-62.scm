;; 問題2.62
(load "./ch2/exercise2-61.scm")

(define (union-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (union-set (cdr set1)
                                     (cdr set2))))
            ((< x1 x2)
             (cons x1
                   (union-set (cdr set1) set2)))
            ((< x2 x1)
             (cons x2
                   (union-set set1 (cdr set2))))))))

(define set1 '(1 2 3 4 5))
(define set2 '(3 4 5 6 7))

(union-set set1 set2)
