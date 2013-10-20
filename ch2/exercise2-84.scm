;; 問題2.84
; raise演算を使って，お引数を同じ型になるまで強制変換するようにせよ

(load "./ch2/exercise2-83")

;;http://www.serendip.ws/archives/1087
(define (higher-type type1 type2)
  ;;型の型、新しい型を定義したらこのリストに追加する
  (let ((tower '(complex rational scheme-number)))
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
;(define r (make-real 2.0))
(define ra (make-rational 1 2))
(define c (make-complex-from-real-imag 1 3))

;(higher-type (type-tag i) (type-tag r))
;(higher-type (type-tag r) (type-tag ra))
(higher-type (type-tag i) (type-tag ra))
(higher-type (type-tag i) (type-tag c))

;(coerce-higher-type (list r i))
(coerce-higher-type (list c ra))
(coerce-higher-type (list ra i))

;(add i r)
;;gosh> (real . 4.0)
(add i ra)
;;gosh> (rational 5 . 2)
;(add r c)
;;gosh> (complex rectangular 3.0 . 3)
(add ra c)
