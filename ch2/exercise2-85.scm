;; 問題2.85

(load "./ch2/exercise2-84.scm")

;;apply-genericを使わずにprojectを定義
;;http://www.serendip.ws/archives/1107
(define (install-project-package)
  (define (complex->rational x)
    (make-rational (real-part x) 1))
;  (define (complex->real x)
;    (make-real (real-part x)))
;  (define (real->rational x)
;    (make-rational (x->integer x) 1))
  (define (rational->scheme x)
    (let ((n (car x))
          (d (cdr x)))
      (make-scheme-number (round (/ n d)))))
  (put 'project '(complex) complex->rational)
  ;(put 'project 'real real->rational)
  (put 'project '(rational) rational->scheme)
  'done)
(install-project-package)

(define (project x)
    (let ((proc (get 'project (list (type-tag x)))))
      (if proc
          (proc (contents x))
          false)))

(project (make-rational 1 2))
(project (make-complex-from-real-imag 1 3))

;;apply-genericを使わずにraiseを定義
(define (raise x)
  (let ((proc (get 'raise (list (type-tag x)))))
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
            (if (eq? type1 type2)
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
;(define rel (make-real 3.0))
(define cpx (make-complex-from-real-imag 2 0))
(drop int)
;;gosh> (integer . 2)
(drop rat)
;gosh> (rational 1 . 2)
;(drop rel)
;gosh> (integer . 3)
(drop cpx)
;gosh> (integer . 2)
(add int int)
;gosh> (integer . 4)
;(add int rel)
;gosh> (integer . 5)
(add int cpx)
;gosh> (integer . 4)
;(add rat rel)
;gosh> (real . 3.5)
;(add cpx rel)
;gosh> (integer . 5)
(add rat rat)
(add cpx cpx)

(add int rat)

(add rat cpx)

;;実数から有理数への変換
;;そもそも我々の扱っている実数は有限なので全部有理数にできるよね？
;;真面目にやるなら、..ある程度妥協、εより小さければドロップ、
;;めんどくさいですね。
