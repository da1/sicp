;; 問題2.61
; 順序付けられた表現でのadjoin-set
(load "./ch2/2-3-3_Example_Representing_Sets.scm")

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else
          (cons (car set) (adjoin-set x (cdr set))))))

(define set1 '(10 20 30 40))

(adjoin-set 10 set1)
(adjoin-set 0 set1)
(adjoin-set 50 set1)
(adjoin-set 25 set1)
