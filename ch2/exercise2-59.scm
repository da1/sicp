;; 問題2.59
(load "./ch2/2-3-3_Example_Representing_Sets.scm")

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (cons (car set1) (union-set (cdr set1) set2)))))

(define set1 '(1 2 3 4 5))
(define set2 '(3 4 5 6 7))

(union-set set1 set2)
