;; 2.5.2 異なる方データの統合

(load "./ch2/2-5_system_with_generic_operations.scm")

;;強制型変換 coercion
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

;;強制変換型手続きを、二つの型で引く特別の強制型変換表に設定する
; (put-coercion 'scheme-number 'complex scheme-number->complex)
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




;;2.86
;;Exercise 2.86.  Suppose we want to handle complex numbers whose real parts, imaginary parts, magnitudes, and angles can be either ordinary numbers, rational numbers, or other numbers we might wish to add to the system. Describe and implement the changes to the system needed to accommodate this. You will have to define operations such as sine and cosine that are generic over ordinary numbers and rational numbers.
