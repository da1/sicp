;; 問題2.6
;; Church数

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; oneとtwoを，zeroとadd-1を使わずに定義せよ
;; 加算手続き+をadd-1をつかわずに定義せよ

;; TAPLから定義を引用 (日本語版p.46)
;; c0 = λs. λz. z;
;; c1 = λs. λz. s z;
;; c2 = λs. λz. s (s z);
(define one (lambda (s) (lambda (z) (s z))))
(define two (lambda (s) (lambda (z) (s (s z)))))

;; plus = λm. λn. λs. λz. m s (n s z);
(define plus
  (lambda (m)
    (lambda (n)
      (lambda (s)
        (lambda (z)
          ((m s) ((n s) z)))))))

(define (print-church c)
  (define (inc x) (+ x 1))
  ((c inc) 0))

zero
(print-church zero)

one
(print-church one)

two
(print-church two)

(print-church (add-1 zero))
(print-church ((plus zero) one))
(print-church ((plus one) one))
