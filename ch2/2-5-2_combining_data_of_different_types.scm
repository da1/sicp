;;強制型変換 coercion
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

;;強制変換型手続きを、二つの型で引く特別の強制型変換表に設定する
(put-coercion 'scheme-number 'complex scheme-number->complex)
;;put-coercionの実装をあとで

;;強制型変換の表をしらべて、変換する
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
		(if (equal? type1 type2)
		    (error "No method for these same types" (list op type-tags)))
		(let ((t1->t2 (get-coercion type1 type2))
		      (t2->t1 (get-coercion type2 type1)))
		  (cond (t1->t2
			 (apply-generic op (t1->t2 a1) a2))
			(t2->t1
			 (apply-generic op a1 (t2->t1 a2)))
			(else
			 (error "No method for these types"
				(list op type-tags))))))
	      (error "No method for these types"
		     (list op type-tags)))))))

(define get-coercion (operation-table 'lookup-proc))
(define put-coercion (operation-table 'insert-proc!))

;;2.81
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(exp 2 2)

(exp (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 2 2))

;;a
;;無限ループで帰ってこない
;;

;;b
;;なんかよくないよね
;;なにかするべきだけど強制型変換はないね

;;c
;;同じtypeのときはerror

;;2.82
;;整数・複素数の演算で、実数・複素数の演算は定義されているが、複素数・複素数の演算が定義されていないとき、一気に複素数・複素数まで上げてしまうと手続きが定義されてないよとなってしまう。
;;3引数以上のときは
;;(C R I)
;;R->C, I->Cが定義されているなら変換して終わり
;;(R C I)
;;Rに合わせるのは無理、次にCをみて上と同じようにCに統一する

;;強制型変換の表をしらべて、変換する
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
		(if (equal? type1 type2)
		    (error "No method for these same types" (list op type-tags)))
		(let ((t1->t2 (get-coercion type1 type2))
		      (t2->t1 (get-coercion type2 type1)))
		  (cond (t1->t2
			 (apply-generic op (t1->t2 a1) a2))
			(t2->t1
			 (apply-generic op a1 (t2->t1 a2)))
			(else
			 (error "No method for these types"
				(list op type-tags))))))
	      (error "No method for these types"
		     (list op type-tags)))))))

(define (apply-generic op . args)
  (define (coerce-all args target-type-tag) ;;args各要素をすべてtarget-type-tagに変える
    (if (null? args)
	'()
	(let ((proc (get-coercion (type-tag (car args)) target-type-tag)))
	  (if proc
	      (cons (proc (car args)) (coerce-all (cdr args) target-type-tag))
	      (cons (car args) (coerce-all (cdr args) target-type-tag))))))
  (let ((type-tags (map type-tag args)))
    (define (coercion-all-first-type-tag types)
      (let ((first-type (car types)))
	(if (null? first-type)
	    false
	    (let ((first-type-args (coerce-all args first-type)))
	      (let ((proc (get op (map type-tag first-type-args))))
		(if proc
		    (apply proc (map contents first-type-args))
		    (coercion-all-first-type-tag (cdr types))))))))
    (coercion-all-first-type-tag type-tags)))

(define num1 (make-scheme-number 1))
(define num2 (make-from-real-imag 1 2))

(put 'add '(scheme-number scheme-number scheme-number)
     (lambda (x y z) (+ x y z)))

(put 'add '(complex complex complex)
     (lambda (x y z) (add (add (cons 'complex x)
                               (cons 'complex y))
                          (cons 'complex z))))

(apply-generic 'add 2 (make-complex-from-real-imag 2 3))
; => (complex rectangular 4 . 3)
(apply-generic 'add 2 (make-complex-from-real-imag 2 3) 3)
; => (complex rectangular 7 . 3)
(apply-generic 'add (make-complex-from-real-imag 2 3) 3 -5)
; => (complex rectangular 0 . 3)

;;2.83
;;http://d.hatena.ne.jp/awacio/20101017/1287326044
(define scheme-num (make-scheme-number 2))
scheme-num
(define rational-num (raise scheme-num))
rational-num
(define real-num (raise rational-num))
real-num
(define complex-num (raise real-num))
complex-num

;;2.84
;;http://www.serendip.ws/archives/1087
(define (higher-type type1 type2)
  ;;型の型、新しい型を定義したらこのリストに追加する
  (let ((tower '(complex real rational scheme-number)))
    (define (iter twr)
      (if (null? twr)
	  false
	  (cond ((eq? type1 (car twr)) type1)
		((eq? type2 (car twr)) type2)
		(else (iter (cdr twr))))))
      (iter tower)))

;;２つの型の値のリストを受け取り、高い型の方に統一して返す
(define (coerce-higher-type items)
  (let ((item1 (car items))
	(item2 (cadr items)))
    (let ((type1 (type-tag item1))
	  (type2 (type-tag item2)))
      (if (eq? type1 type2)
	  items
	  (let ((tag (higher-type type1 type2)))
	    (if (eq? tag type1)
		(coerce-higher-type (list item1 (raise item2)))
		(coerce-higher-type (list (raise item1) item2))))))))
(define a1 (make-scheme-number 1))
(define a2 (make-complex-from-real-imag 1 1))
(define a3 (make-rational 1 2))
(coerce-higher-type (list a1 a2 a3))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      ;;2個以上の引数に対応するには
	      ;;mapでtypesみたいなのにする
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags)))
		;;typeが全部同じか調べる
		(if (eq? type1 type2)
		    (error "E1. No method for these types" (list op type-tags))
		    ;;coerce-higher-typeを複数の型から一番高いのを返すようにする
		    (let ((coerced-args (coerce-higher-type args)))
		      (let ((proc (get op (map type-tag coerced-args))))
			(if proc
			    (apply proc (map contents coerced-args))
			    (error "E2.No method for these types" (list op type-tags)))))))
	      (error "E3. No method for these types" (list op type-tags)))))))

(define i 2)
(define r (make-real 2.0))
(define ra (make-rational 1 2))
(define c (make-complex-from-real-imag 1 3))

(higher-type (type-tag i) (type-tag r))
(higher-type (type-tag r) (type-tag ra))
(higher-type (type-tag i) (type-tag ra))
(higher-type (type-tag i) (type-tag c))

(coerce-higher-type (list r i))
(coerce-higher-type (list c ra))
(coerce-higher-type (list ra i))

(add i r)
;;gosh> (real . 4.0)
(add i ra)
;;gosh> (rational 5 . 2)
(add r c)
;;gosh> (complex rectangular 3.0 . 3)

;;2.85
;;http://www.serendip.ws/archives/1107
(project c)
(project ra)
(project r)

;;apply-genericを使わずにprojectを定義
(define (project x)
    (let ((proc (get 'project (type-tag x))))
      (if proc
          (proc (contents x))
          false)))

;;apply-genericを使わずにraiseを定義
(define (raise x)
  (let ((proc (get 'raise (type-tag x))))
	(if proc
	    (proc (contents x))
	    false)))

;; project を用いて drop を定義
(define (drop x)
  (if (pair? x)
      (let ((projected (project x)))
        (if projected
            (if (equal? (raise projected) x)
                (drop projected)
                x)
            x)
        )
      x ;through, for passing #t, etc.
      ))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args))) ;;★
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags)))
                (if (equ? type1 type2)
                    (error "No method for these types" (list op type-tags))
                    (let ((args-coerced (coerce-higher-type args)))
                      (let ((type-tags (map type-tag args-coerced)))
                        (let ((proc (get op type-tags)))
                          (if proc
                              (drop (apply proc (map contents args-coerced))) ;;★
                              (error "No method for these types" (list op type-tags))
                              )
                          )))))
              false)))))

(define int (make-scheme-number 2))
(define rat (make-rational 2 4))
(define rel (make-real 3.0))
(define cpx (make-complex-from-real-imag 2 0))
(drop int)
;;gosh> (integer . 2)
(drop rat)
;gosh> (rational 1 . 2)
(drop rel)
;gosh> (integer . 3)
(drop cpx)
;gosh> (integer . 2)
(add int int)
;gosh> (integer . 4)
(add int rel)
;gosh> (integer . 5)
(add int cpx)
;gosh> (integer . 4)
(add rat rel)
;gosh> (real . 3.5)
(add cpx rel)
;gosh> (integer . 5)

;;実数から有理数への変換
;;そもそも我々の扱っている実数は有限なので全部有理数にできるよね？
;;真面目にやるなら、..ある程度妥協、εより小さければドロップ、
;;めんどくさいですね。

;;2.86
;;Exercise 2.86.  Suppose we want to handle complex numbers whose real parts, imaginary parts, magnitudes, and angles can be either ordinary numbers, rational numbers, or other numbers we might wish to add to the system. Describe and implement the changes to the system needed to accommodate this. You will have to define operations such as sine and cosine that are generic over ordinary numbers and rational numbers.
