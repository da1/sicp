;; 問題2.90
;;http://www.serendip.ws/archives/1137
;;薄い多項式

(load "./ch2/2-5-2_combining_data_of_different_types.scm")
(load "./ch2/2-5-3_example_symbolic_algebra.scm")
(define (install-sparse-term-package)
  ;; 内部手続き
  (define (make-sparse-term order coeff) (list order coeff))
  (define (the-empty-sparse-termlist) '())
  (define (empty-sparse-termlist? term-list) (null? term-list))
  (define (first-sparse-term term-list) (car term-list))
  (define (rest-sparse-terms term-list) (cdr term-list))
  (define (order-sparse term) (car term))
  (define (coeff-sparse term) (cadr term))
  ;;2.96
  (define (max-order-sparse term-list) (order-sparse (first-sparse-term term-list)))
  (define (max-coeff-sparse term-list) (coeff-sparse (first-sparse-term term-list)))
  (define (adjoin-sparse-term term term-list)
    (if (=zero? (coeff-sparse term))
      term-list
      (cons term term-list)))
  (define (=zero-sparse-term? L)
    (or (empty-sparse-termlist? L)
        (and (=zero? (coeff-sparse (first-sparse-term L)))
             (=zero-sparse-term? (rest-sparse-terms L)))))
  (define (add-sparse-terms L1 L2)
    (cond ((empty-sparse-termlist? L1) L2)
          ((empty-sparse-termlist? L2) L1)
          (else
            (let ((t1 (first-sparse-term L1)) (t2 (first-sparse-term L2)))
              (cond ((> (order-sparse t1) (order-sparse t2))
                     (adjoin-sparse-term
                       t1 (add-sparse-terms (rest-sparse-terms L1) L2)))
                    ((< (order-sparse t1) (order-sparse t2))
                     (adjoin-sparse-term
                       t2 (add-sparse-terms L1 (rest-sparse-terms L2))))
                    (else
                      (adjoin-sparse-term
                        (make-sparse-term (order-sparse t1)
                                          (add (coeff-sparse t1) (coeff-sparse t2)))
                        (add-sparse-terms (rest-sparse-terms L1)
                                          (rest-sparse-terms L2)))))))))
  (define (mul-sparse-terms L1 L2)
    (if (empty-sparse-termlist? L1)
      (the-empty-sparse-termlist)
      (add-sparse-terms (mul-sparse-term-by-all-sparse-terms
                          (first-sparse-term L1) L2)
                        (mul-sparse-terms (rest-sparse-terms L1) L2))))
  (define (mul-sparse-term-by-all-sparse-terms t1 L)
    (if (empty-sparse-termlist? L)
      (the-empty-sparse-termlist)
      (let ((t2 (first-sparse-term L)))
        (adjoin-sparse-term
          (make-sparse-term (+ (order-sparse t1) (order-sparse t2))
                            (mul (coeff-sparse t1) (coeff-sparse t2)))
          (mul-sparse-term-by-all-sparse-terms t1 (rest-sparse-terms L))))))
  (define (negate-sparse-term L)
    (if (empty-sparse-termlist? L)
      (the-empty-sparse-termlist)
      (let ((t (first-sparse-term L)))
        (adjoin-sparse-term
          (make-sparse-term (order-sparse t)
                            (mul (make-scheme-number -1) (coeff-sparse t)))
          (negate-sparse-term (rest-sparse-terms L))))))
  ;;2.91
  (define (div-sparse-terms L1 L2)
    (if (empty-sparse-termlist? L1)
      (list (the-empty-sparse-termlist) (the-empty-sparse-termlist))
      (let ((t1 (first-sparse-term L1))
            (t2 (first-sparse-term L2)))
        (if (> (order-sparse t2) (order-sparse t1))
          (list (the-empty-sparse-termlist) L1)
          (let ((new-c (div (coeff-sparse t1) (coeff-sparse t2)))
                (new-o (- (order-sparse t1) (order-sparse t2))))
            (let ((rest-of-result
                    ; <結果の残りを再帰的に計算する>
                    (div-sparse-terms 
                      (sub-sparse-terms L1 (mul-sparse-terms (list (make-sparse-term new-o new-c)) L2))
                      L2)
                    ))
              ; <完全な結果を形成する>
              (list (add-sparse-terms (list (make-sparse-term new-o new-c))
                                      (car rest-of-result))
                    (cadr rest-of-result))
              ))))))
  (define (sub-sparse-terms L1 L2)
    (add-sparse-terms L1 (negate-sparse-term L2)))
  ;; 外部とのインターフェース
  (define (tag x) (attach-tag 'sparse-term x))
  (put '=zero-term? '(sparse-term) =zero-sparse-term?)
  (put 'order '(sparse-term) order-sparse)
  (put 'add-terms '(sparse-term sparse-term)
       (lambda (x y) (tag (add-sparse-terms x y))))
  (put 'mul-terms '(sparse-term sparse-term)
       (lambda (x y) (tag (mul-sparse-terms x y))))
  (put 'div-terms '(sparse-term sparse-term)
       (lambda (x y) (tag (div-sparse-terms x y))))
  (put 'negate-term '(sparse-term)
       (lambda (x) (tag (negate-sparse-term x))))
  ;;2.96
  (put 'max-order-terms '(sparse-term)
       (lambda (x) (max-order-sparse x)))
  (put 'max-coeff-terms '(sparse-term)
       (lambda (x) (max-coeff-sparse x)))
  (put 'make-from-sparse 'sparse-term
       (lambda (sparse-term-list) (tag sparse-term-list)))
  (put 'make-from-dense 'sparse-term
       (lambda (dense-term-list) (tag (dense->sparse dense-term-list))))
  'done)

(define (make-sparse-term term-list)
  ((get 'make-from-sparse 'sparse-term) term-list))
(define (max-order term-list)
  (apply-generic 'max-order-terms term-list))
(define (max-coeff term-list)
  (apply-generic 'max-coeff-terms term-list))

(install-sparse-term-package)

;;濃い多項式
(define (install-dense-term-package)
  ;; 内部手続き
  (define (adjoin-dense-term term term-list)
    (cons term term-list))
  (define (the-empty-dense-termlist) '())
  (define (empty-dense-termlist? term-list) (null? term-list))
  (define (first-dense-term term-list) (car term-list))
  (define (rest-dense-terms term-list) (cdr term-list))
  (define (order-dense-term term-list) (length (rest-dense-terms term-list)))
  (define (coeff-dense-term term-list) (first-dense-term term-list))
  (define (=zero-dense-term? L)
    (or (empty-dense-termlist? L)
        (and (=zero? (coeff-dense-term L))
             (=zero-dense-term? (rest-dense-terms L)))))
  (define (normalize-dense-term L)
    (cond ((empty-dense-termlist? L) L)
          ((=zero? (first-dense-term L))
           (normalize-dense-term (rest-dense-terms L)))
          (else L)))
  (define (add-dense-terms L1 L2)
    (define (add-rterms R1 R2)
      (cond ((empty-dense-termlist? R1) R2)
            ((empty-dense-termlist? R2) R1)
            (else
              (adjoin-dense-term (add (first-dense-term R1)
                                      (first-dense-term R2))
                                 (add-rterms (cdr R1) (cdr R2))))))
    (cond ((empty-dense-termlist? L1) L2)
          ((empty-dense-termlist? L2) L1)
          (else
            (normalize-dense-term (reverse (add-rterms (reverse L1)
                                                       (reverse L2)))))))
  (define (expand-dense-term L n)
    (if (= n 0)
      L
      (expand-dense-term
        (adjoin-dense-term 0 L) (- n 1))))
  (define (mul-dense-terms L1 L2)
    (define (mul-dense-terms-sub n L1 L2)
      (if (= n 0)
        (mul-dense-term-by-all-dense-terms 0 (first-dense-term L1) L2)
        (add-dense-terms
          (mul-dense-term-by-all-dense-terms n (first-dense-term L1) L2)
          (mul-dense-terms-sub (- n 1) (rest-dense-terms L1) L2))))
    (if (or (empty-dense-termlist? L1) (empty-dense-termlist? L2))
      (the-empty-dense-termlist)
      (mul-dense-terms-sub (order-dense-term L1) L1 L2)))
  (define (mul-dense-term-by-all-dense-terms n t1 L)
    (reverse (expand-dense-term (map (lambda (t) (mul t1 t)) (reverse L)) n)))
  (define (negate-dense-term L) (map (lambda (x) (mul x (make-scheme-number -1))) L))
  ;;2.91 div
  (define (div-dense-terms L1 L2)
    (if (empty-dense-termlist? L1)
      (list (the-empty-dense-termlist) (the-empty-dense-termlist))
      (let ((t1 (first-dense-term L1)) (t2 (first-dense-term L2)) (o1 (order-dense-term L1)) (o2 (order-dense-term L2)))
        (if (> o2 o1)
          (list (the-empty-dense-termlist) L1)
          (let ((new-c (div t1 t2))
                (new-o (- o1 o2)))
            (let ((rest-of-result 
                    (div-dense-terms 
                      (sub-dense-terms
                        L1
                        (mul-dense-terms (make-list-sub new-o new-c) L2))
                      L2)))
              (list (add-dense-terms (make-list-sub new-o new-c)
                                     (car rest-of-result))
                    (cadr rest-of-result))
              ))))))
  (define (make-list-sub o c)
    (cons c (make-list o 0)))
  (define (sub-dense-terms L1 L2)
    (add-dense-terms L1 (negate-dense-term L2)))
  ;; 外部とのインターフェース
  (define (tag x) (attach-tag 'dense-term x))
  (put '=zero-term? '(dense-term) =zero-dense-term?)
  (put 'add-terms '(dense-term dense-term)
       (lambda (x y) (tag (add-dense-terms x y))))
  (put 'mul-terms '(dense-term dense-term)
       (lambda (x y) (tag (mul-dense-terms x y))))
  (put 'div-terms '(dense-term dense-term)
       (lambda (x y) (tag (div-dense-terms x y))))
  (put 'negate-term '(dense-term)
       (lambda (x) (tag (negate-dense-term x))))
  (put 'make-from-sparse 'dense-term
       (lambda (sparse-term-list) (tag (sparse->dense sparse-term-list))))
  (put 'make-from-dense 'dense-term
       (lambda (dense-term-list) (tag dense-term-list)))
  'done)

(define (make-dense-term term-list)
  ((get 'make-from-dense 'dense-term) term-list))

(install-dense-term-package)

;;濃い多項式から薄い多項式への変換
(define (dense->sparse term-list)
  ;;途中結果、次数、濃い多項式
  (define (iter result i term-list)
    (if (null? term-list)
        result
        (iter (cons (list i (car term-list)) result) (+ i 1) (cdr term-list))))
  (iter '() 0 (reverse term-list)))
;;薄い多項式から濃い多項式への変換
(define (sparse->dense term-list)
  (define (iter result i term-list)
    (if (null? term-list)
        result
        (let ((term (car term-list)))
             (let ((j (car term)))
                  (if (= i j)
                      (iter (cons (cadr term) result) (+ i 1) (cdr term-list))
                      (iter (cons (make-scheme-number 0) result) (+ i 1) term-list))))))
  (iter '() 0 (reverse term-list)))
(put-coercion 'dense-term 'sparse-term
              (lambda (d) (make-sparse-term (dense->sparse (contents d)))))
(put-coercion 'sparse-term 'dense-term
              (lambda (s) (make-dense-term (sparse->dense (contents s)))))
(put-coercion 'dense-term 'dense-term identity)
(put-coercion 'sparse-term 'sparse-term identity)

;;汎用多項式演算パッケージ
(define (install-palynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;;2.96
  (define (max-order-poly p) (max-order p))
  (define (max-coeff-poly p) (max-coeff p))
  ;;
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (=zero-poly? p) (=zero-term? (term-list p)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY" (list p1 p2))))
  ;;2.91
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (div-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- DIV-POLY" (list p1 p2))))
  (define (negate-poly p)
    (make-poly (variable p) (negate-term (term-list p))))
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))
  ;2.93
  (define (gcd-terms a b)
    ;    (display (list 'gcd-terms a b))
    ;    (newline)
    (if (empty-termlist? b)
      a
      ;        (gcd-terms b (remainder-terms a b))))
      (gcd-terms b (pseudoremainder-terms a b))))
  (define (remainder-terms a b)
    ;    (cadr #?=(div-terms a b)))
    (let ((div-value (div-terms a b)))
      (cons (car div-value) (caddr div-value)))
    )
  (define (pseudoremainder-terms a b)
    (let ((o1 (max-order-poly #?=a)) (o2 (max-order-poly #?=b)) (c (max-coeff-poly b)))
      (let ((integerizing-factor #?=(expt c (+ 1 (- o1 o2)))))
        (let ((div-value #?=(div-terms
                              #?=(mul a
                                      #?=(make-sparse-term (list (list 0 integerizing-factor)))) 
                              b)))
          (cons (car div-value) (caddr div-value))))))
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (gcd-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- GCD-POLY" (list p1 p2))))
  (define (empty-termlist? term-list)
    ;(null? term-list))
    (if (null? term-list)
      true
      (null? (cdr term-list))))
  (define (the-empty-termlist) '())

  ;; 外部とのインターフェース
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  ;;2.91
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  ;;2.96
  (put 'max-order '(polynomial)
       (lambda (x) (max-order-poly x)))
  (put 'max-coeff '(polynomial)
       (lambda (x) (max-coeff-poly x)))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-palynomial-package)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2
                      (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                      (apply-generic op a1 (t2->t1 a2)))
                    (else (error "No method for these types" (list op type-tags))))))
          (error "No method for these types" (list o type-tags)))))))

(define (add-terms x y) (apply-generic 'add-terms x y))
(define (mul-terms x y) (apply-generic 'mul-terms x y))

(define p1 (make-polynomial 'x (make-sparse-term '((1 2) (0 1)))))
p1
;(polynomial x sparse-term (1 2) (0 1))
(define p2 (make-polynomial 'x (make-dense-term '(-1 -1))))
p2
;(polynomial x dense-term -1 -1)
(add p1 p1)
;;gosh> (polynomial x sparse-term (1 4) (0 2))
(add p2 p2)
;;gosh> (polynomial x dense-term -2 -2)
(add p1 p2)
;;gosh> (polynomial x dense-term 1 0)
(add p2 p1)
;;gosh> (polynomial x sparse-term (1 1))
(mul p1 p1)
;;gosh> (polynomial x sparse-term (2 4) (1 4) (0 1))
(mul p2 p2)
;;gosh> (polynomial x dense-term 1 2 1)
(mul p1 p2)
;;gosh> (polynomial x dense-term -2 -3 -1)
(mul p2 p1)
;;gosh> (polynomial x sparse-term (2 -2) (1 -3) (0 -1))
