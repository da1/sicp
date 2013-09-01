;; 問題2.57
; 任意個の項と和が扱えるようにせよ

(load "./ch2/exercise2-56.scm")
;(load "./ch2/2-3-2_Hierarchical_Structures.scm")

; http://www.serendip.ws/archives/893
; augend と multiplicand だけ変更する
(define (augend s)
  (if (null? (cdddr s))
    (caddr s)
    (cons '+ (cddr s))))

(augend '(+ 1 2 3))
(augend '(+ 1 2))

(define (multiplicand p)
  (if (null? (cdddr p))
    (caddr p)
    (cons '* (cddr p))))

(multiplicand '(* 1 2 3))
(multiplicand '(* 1 2))

(deriv '(* x y (+ x 3)) 'x)

