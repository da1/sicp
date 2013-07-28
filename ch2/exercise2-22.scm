;; 問題2.22
;; Louis Reasoner は問題2.21のはじめのsquare-listを書きなおし，反復プロセスを生成するようにした．
(load "./utils.scm")

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
  (iter items nil))

(square-list (list 1 2 3 4))
;; (1 2 3 4) ()
;; (2 3 4) (1)
;; (3 4) (4 1)
;; (4) (9 4 1)
;; () (16 9 4 1)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items nil))

(square-list (list 1 2 3 4))
;; (1 2 3 4) ()
;; (2 3 4) (() . 1)
;; (3 4) ((() . 1) . 4)
;; (4) (((() . 1) . 4) . 9)
;; () ((((() . 1) . 4) . 9) . 16)

