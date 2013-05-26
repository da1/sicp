;; 問題 1.17
;; 対数的ステップ数の乗算手続きを設計せよ

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(* 3 4)

(define (double n)
  (+ n n))
(define (halve n)
  (if (even? n)
      (/ n 2)
      (error "not even" n)))

(double 3)
(halve 10)
(halve 5)

(define (* a b)
  (cond ((= b 0) 0)
    ((even? b) (double (* a (halve b))))
    (else (+ a (* a (- b 1))))))
(* 3 4)
(* 12 56)
