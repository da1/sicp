;; 2.5 汎用演算のシステム

;2.5.1 汎用算術演算
; 汎用算術演算の設計作業は，汎用複素数演算の背系に似ている
; 汎用のaddは，通常の数については基本の加算 + のように，有理数に対してはadd-ratのように，複素数に対してはadd-complexのように働いてほしい

(load "./ch2/2-4_Multiple_Representations_for_Abstract_Data.scm")

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (zero? x) (apply-generic 'zero? x))
;;問題2.81用
(define (exp x y) (apply-generic 'exp x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put 'zero? '(scheme-number)
       (lambda (x) (= x 0)))
  ;; 問題2.81
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  ;; 2.83
  (put 'raise '(scheme-number)
       (lambda (x)
         (make-rational (contents x) 1)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
'done)
(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;;sample
(define n (make-scheme-number 10))
(define m (make-scheme-number 5))
(add n m)
(sub n m)
(mul n m)
(div n m)

;; 有理数算術演算のパッケージ
(define (install-rational-package)
  ;; 内部手続き
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; システムの他の部分へのインタフェース
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equal? x y)))
  (put 'zero? '(rational)
       (lambda (x) (= 0 (numer x))))
  ;; 2.83
  ;;;複素数への強制型変換
  (put 'raise '(rational)
       (lambda (x)
         (make-complex-from-real-imag (contents x) 0)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define r1 (make-rational 2 5))
(define r2 (make-rational 1 4))
(add r1 r2)
(sub r1 r2)
(mul r1 r2)
(div r1 r2)

;; 複素数のパッケージ
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (=zero-complex? z1)
    (and (= (real-part z1) 0)
         (= (imag-part z1) 0)))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (x y) (tag (make-from-mag-ang x y))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
       (lambda (x y) (equal? x y)))
  (put 'zero? '(complex)
       (lambda (x) (=zero-complex? x)))
  ;; 2.83
  ;;;複素数への強制型変換
  (put 'raise '(real)
       (lambda (x)
         (make-complex-from-real-imag (contents x) 0)))
  'done)
(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang x y)
  ((get 'make-from-mag-ang 'complex) x y))

(define ri1 (make-complex-from-real-imag 3 4))
(define ri2 (make-complex-from-real-imag -1 2))
(add ri1 ri2)
(sub ri1 ri2)
(mul ri1 ri2)
(div ri1 ri2)
(define ma1 (make-complex-from-mag-ang 2 3.14))
(define ma2 (make-complex-from-mag-ang 1 1.57))
(add ma1 ma2)
(sub ma1 ma2)
(mul ma1 ma2)
(div ma1 ma2)

