;; 2.3.2あたりを見ること
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;;微分
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	(else (error "unknown expression type -- DERIV" exp))))

;;新しい微分
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) (operands exp)
	       var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; a.  Explain what was done above. 
;; Why can't we assimilate the predicates number? and same-variable? into the data-directed dispatch?
;;型を区別するタグがないからだとか
;;(operator operands)に分解できるものはoperatorに対する演算を探して処理ができるが、そうでないので無理
;;http://oss.timedia.co.jp/show/SICP/ex-2.73
;;http://sicp.naochan.com/memo.pl?p=%CC%E4%C2%EA2.73

(define (install-deriv-package)
  (define (addend operands)
    (car operands))
  (define (augend operands)
    (if (null? (cddr operands))
	(cadr operands) ;後ろがないなら一個数値をとる
	(cons '+ (cdr operands)))) ;続きがあるなら'+をつけて新しい式にする
  (define (multiplier operands)
    (car operands))
  (define (multiplicand operands)
    (if (null? (cddr operands))
	(cadr operands)
	(cons '* (cdr operands))))
  (define (deriv-sum exp var)
    (make-sum (deriv (append exp) var)
	      (deriv (augend exp) var)))
  (define (deriv-prod exp var)
    (make-sum (make-product (multiplicand exp) (deriv (multiplier exp) var))
	      (make-product (multiplier exp) (deriv (multiplicand exp) var))))
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-prod)
  'done)
(install-deriv-package)
