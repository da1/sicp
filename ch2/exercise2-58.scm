;; 問題2.58
; 前置演算子でなく中置き表現の代数式を微分するにはどうしたらいいか

; 式は完全にかっこで囲まれており，+ や *は常に2つの項をとるものとする

(load "./ch2/2-3-2_Hierarchical_Structures.scm")

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s)
  (car s))
(define (augend s)
  (caddr s))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(deriv '(x + (3 * (x + (y + 2)))) 'x)
