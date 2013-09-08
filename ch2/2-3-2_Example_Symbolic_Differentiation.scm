;; 2.3.2 例: 記号微分

;; 抽象データによる微分プログラム

; 変数か?
;(variable? e)

;同じ変数か？
;(same-variable v1 v2)

;和か？
;(sum? e)

; eの加数
;(addend e)

;eの被加数
;(augend e)

;和を構成
;(make-sum a1 a2)

; 積か？
;(product? e)

;乗数
;(multiplier e)

;被乗数
;(multiplicand e)

;積を構成
;(make-product m1 m2)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknown expression type - DERIV" exp))))

;; 代数式の表現
; 変数は記号とする
(define (variable? x) (symbol? x))

; 2つの変数は，それを表現している記号がeq?なら同じである．
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; 和と積はリストとして構成する
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product a1 a2) (list '* a1 a2))

; 和は最初の要素がプラスであるリストである
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;加数は和のリストの第二項である
(define (addend s)
  (cadr s))

;被加数は和のリストの第三項である
(define (augend s)
  (caddr s))

; 積も和とほぼ一緒
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)

; make-sumを修正して簡約化
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

; make-productも同様
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)
