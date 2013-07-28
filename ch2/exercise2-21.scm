;; 問題2.21
;; square-list は引数として数のリストをとりこれらの数の二乗のリストを返す
(load "./utils.scm")

(define (square-list items)
  (if (null? items)
    nil
    (cons (square (car items))
          (square-list (cdr items)))))

(define (square-list items)
  (map square items))

(square-list (list 1 2 3 4))
