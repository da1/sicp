; 拡張問題：有理関数

;2.93

; http://www.serendip.ws/archives/1154
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (cons n d))

  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (=rat-zero? x)
    (= (numer x) 0))
  (define (rat-equ? x y)
    (if (and (= (numer x) (numer y)) (= (denom x) (denom y)))
        #t
        #f))
  (define (negative-rat x)
    (make-rat (- (numer x)) (denom x)))
  ;;
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put '=zero? '(rational)
       (lambda (x) (=rat-zero? x)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (rat-equ? x y)))
  (put 'negative '(rational)
       (lambda (x) (tag (negative-rat x))))
  'done)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))


;(define p1 (make-polynomial 'x '((2 1) (0 1))))
;(define p2 (make-polynomial 'x '((3 1) (0 1))))
(define p1 (make-polynomial 'x (make-sparse-term '((2 1) (0 1)))))
(define p2 (make-polynomial 'x (make-sparse-term '((3 1) (0 1)))))
(define rf (make-rational p2 p1))
(add rf rf)


;2.94
(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

;; x^4 - x^3 - 2x^2 + 2x
(define p1 (make-polynomial 'x (make-sparse-term '((4 1) (3 -1) (2 -2) (1 2))))) 
p1
;(polynomial x (4 1) (3 -1) (2 -2) (1 2))
;; x^3 - x
(define p2 (make-polynomial 'x (make-sparse-term '((3 1) (1 -1))))) 
p2
;(polynomial x (3 1) (1 -1))
(greatest-common-divisor p1 p2)
;gosh> (polynomial x (2 -1) (1 1))

;2.95
(define p1 (make-polynomial 'x (make-sparse-term '((2 1) (1 -2) (0 1)))))
(define p2 (make-polynomial 'x (make-sparse-term '((2 11) (0 7)))))
(define p3 (make-polynomial 'x (make-sparse-term '((1 13) (0 5)))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(greatest-common-divisor q1 q2)

;13x^4 + ???
;11x^4 + ???
;11/13を上式にかけるて下式を引くとx^4の項が消せるが、上式の他の項に11/13がかかる。
;そういう計算を他の項でもやっていくと結果が変なことになる

