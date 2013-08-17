;; 問題2.28
; 引数として木をとって，その要素が木のすべての葉を左から右の順であるリストを返す手続きfringeを書け

(define x (list (list 1 2) (list 3 4)))

(define (fringe items)
  (cond ((null? items) ())
        ((not (pair? items)) (list items))
        (else
          (append (fringe (car items))
                  (fringe (cdr items))))))

(fringe (list 1 2))

(fringe x)
; (1 2 3 4)

(fringe (list x x))
; (1 2 3 4 1 2 3 4)
