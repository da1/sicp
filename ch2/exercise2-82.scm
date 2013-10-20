;; 問題2.82

(load "./ch2/2-5-2_combining_data_of_different_types.scm")
(load "./ch2/exercise2-78.scm")

;;整数・複素数の演算で、実数・複素数の演算は定義されているが、複素数・複素数の演算が定義されていないとき、一気に複素数・複素数まで上げてしまうと手続きが定義されてないよとなってしまう。
;;3引数以上のときは
;;(C R I)
;;R->C, I->Cが定義されているなら変換して終わり
;;(R C I)
;;Rに合わせるのは無理、次にCをみて上と同じようにCに統一する

;; http://www.serendip.ws/archives/1070
(define (apply-generic op . args)
  (define (coerce-all args target-type-tag) ;; 引数(args) の各要素を全て target-type-tag 型に変換する（target-type-tag 型のみで構成されるリストを返す）。
    (if (null? args)
      '()
      (let ((proc (get-coercion (type-tag (car args)) target-type-tag)))
        (if proc
          (cons (proc (car args)) (coerce-all (cdr args) target-type-tag))
          (cons (car args) (coerce-all (cdr args) target-type-tag))))))
  (let ((type-tags (map type-tag args))) ;; 引数の型のリスト ex (complex scheme-number complex)
    (define (coercion-all-first-type-tag types) ;; 引数の最初の型に合わせて強制型変換をして演算を試みる
      (if (null? types)
        (error "types null")
      (let ((first-type (car types)))
        (if (null? first-type)
          #f
          (let ((first-type-args (coerce-all args first-type)))
            (let ((proc (get op (map type-tag first-type-args))))
              (if proc
                (apply proc (map contents first-type-args))
                (coercion-all-first-type-tag (cdr types)))))))))
    (coercion-all-first-type-tag type-tags)))

(put 'add '(scheme-number scheme-number scheme-number)
     (lambda (x y z) (+ x y z)))

(put 'add '(complex complex complex)
     (lambda (x y z) (add (add (cons 'complex x)
                               (cons 'complex y))
                          (cons 'complex z))))

(define (add . args) (apply apply-generic (cons 'add args)))

(define z (make-complex-from-real-imag 3 4))

(add 2 2 2)
;gosh> 6
(add z z z)
;gosh> (complex rectangular 9 . 12)
(add z 2 2)
;gosh> (complex rectangular 7 . 4)
(add 2 z 2)
;gosh> (complex rectangular 7 . 4)
(add 2 2 z)
;gosh> (complex rectangular 7 . 4)
(add z z 2)
;gosh> (complex rectangular 8 . 8)
(add z z z)
;gosh> (complex rectangular 9 . 12)
