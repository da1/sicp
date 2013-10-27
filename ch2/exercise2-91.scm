;; 問題2.91
(load "./ch2/exercise2-90.scm")

(define (div-terms x y) (apply-generic 'div-terms x y))

(define pd1 (make-polynomial 'x (make-dense-term '(1 0 0 0 0 -1))))
(define pd2 (make-polynomial 'x (make-dense-term '(1 0 -1))))
(div pd1 pd2)

(define ps1 (make-polynomial 'x (make-sparse-term '((5 1) (0 -1)))))
(define ps2 (make-polynomial 'x (make-sparse-term '((2 1) (0 -1)))))
(div ps1 ps2)

