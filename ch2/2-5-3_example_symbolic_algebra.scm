;;2-5-3　記号代数

(load "./ch2/exercise2-78.scm")

(define (=zero? x) (apply-generic 'zero? x))

;;多項式の算術演算
(define (make-polynomial v t)
  ((get 'make 'polynomial) v t))

;;多項式パッケージ
(define (install-polynomial-package)
  ;;内部手続き
  ;;多項式型の表現
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;;2-3-2
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))
  ;;新しい項を項リストに追加する構成子
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  ;;空の項のリストを返す
  (define (the-empty-termlist) '())
  ;;項リストの最高次の項を取り出す構成子
  (define (first-term term-list) (car term-list))
  ;;最高次の項除いた全てを返す構成子
  (define (rest-terms term-list) (cdr term-list))
  ;;与えられた項リストが空かどうかを調べる述語
  (define (empty-termlist? term-list) (null? term-list))
  ;;項を操作するのに次数と係数から項を構成する構成子
  (define (make-term order coeff) (list order coeff))
  ;;項から次数を取り出す
  (define (order term) (car term))
  ;;項から係数を取り出す
  (define (coeff term) (cadr term))
  ;;項ごとの計算
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))
  ;;2.87
  ;;zero? すべての項で次数がゼロなら真
  (define (zero-terms L)
    (let ((terms (term-list L)))
      (if (empty-termlist? terms)
        false
        (every coeff-zero? terms))))
  (define (coeff-zero? term)
    (if (eq? (coeff term) 0)
      true
      false))
  ;;2.88
  ;;符号反転
  (define (nega-poly p)
    (make-poly (variable p) (nega-term-list (term-list p))))
  (define (nega-term-list terms)
    (map nega-term terms))
  (define (nega-term term)
    (make-term (order term) (mul (coeff term) -1)))
  ;;システムの他の部分とのインタフェース
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'zero? '(polynomial)
       (lambda (p) (zero-terms p)))
  ;;2.88
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (nega-poly p2)))))
  'done)
(install-polynomial-package)

;;;SECTION 2.5.3
(define a (make-polynomial 'x '((5 1) (4 2) (2 3) (1 -2) (0 -5))))
a
;Value 3: (polynomial x (5 1) (4 2) (2 3) (1 -2) (0 -5))
(add a a)
;Value 4: (polynomial x (5 2) (4 4) (2 6) (1 -4) (0 -10))
(define b (make-polynomial 'x '((100 1) (2 2) (0 1))))
b
;Value 5: (polynomial x (100 1) (2 2) (0 1))
(mul b b)
;Value 6: (polynomial x (200 1) (102 4) (100 2) (4 4) (2 4) (0 1))




;;2.92の手前の文章
;;再帰的データ抽象
;;われわれはまだ強制型変換を完全には理解していないといってよいだろう
;;なん..だと..
;;2.92
;;標準形への変換をすればいいようだ
