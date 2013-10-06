; 問題2.73
; 記号微分をデータ手動の形に変換する
(load "./ch2/2-4_Multiple_Representations_for_Abstract_Data.scm")
(load "./ch2/2-3-2_Example_Symbolic_Differentiation.scm")

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
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (define (deriv-prod exp var)
    (make-sum (make-product (multiplicand exp) (deriv (multiplier exp) var))
              (make-product (multiplier exp) (deriv (multiplicand exp) var))))
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-prod)
  'done)
(install-deriv-package)

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)
