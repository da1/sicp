;; 問題2.89
(load "./ch2/2-5-3_example_symbolic_algebra.scm")

;;多項式パッケージ
;;濃い多項式に対応 (polynomial x 6 2 1 2)
(define (install-polynomial-package)
  ;;内部手続き
  ;;多項式型の表現
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
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
    (cons term term-list))
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
  (define (order term) (length (rest-terms term)))
  ;;項から係数を取り出す
  (define (coeff term) (first-term term))
  ;;項ごとの計算
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)) (o1 (order L1)) (o2 (order L2)))
              (cond ((> o1 o2)
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< o1 o2)
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (add t1 t2)
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))

  ;;;;多項式Lを係数0の項を追加して長さnまで伸ばす
  (define (expand-term L n)
    (if (= n 0)
      L
      (expand-term (adjoin-term (make-scheme-number 0) L) (- n 1))))
  (define (mul-terms L1 L2)
    (define (mul-terms-sub n L1 L2)
      (if (= n 0)
        (mul-term-by-all-terms 0 (first-term L1) L2)
        (add-terms (mul-term-by-all-terms n (first-term L1) L2)
                   (mul-terms-sub (- n 1) (rest-terms L1) L2))))
    (if (or (empty-termlist? L1) (empty-termlist? L2))
      (the-empty-termlist)
      (mul-terms-sub (order L1) L1 L2)))
  (define (mul-term-by-all-terms n t1 L)
    (reverse (expand-term (map (lambda (t) (mul t1 t)) (reverse L)) n)))

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
  (put '=zero? '(polynomial)
       (lambda (p) (zero-terms p)))
  ;;2.88
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (nega-poly p2)))))
  'done)
(install-polynomial-package)

(define p1 (make-polynomial 'x '(3 2 0 1)))
(define p2 (make-polynomial 'x '(3 0 1 1)))
(add p1 p2)
;;gosh> (polynomial x 6 2 1 2)
(mul p1 p2)
;;gosh> (polynomial x 9 6 3 8 2 1 1)

