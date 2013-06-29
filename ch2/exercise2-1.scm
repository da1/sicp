;; 問題2.1 正負両方の引数を扱う改良版make-ratを定義せよ
;; make-ratは符号を正規化し，有理数が正なら分子分母とも正，負なら分子だけ負とする

(load "./ch2/2_Building_Abstractions_with_Data.scm")

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons
      (if (< (* n d) 0) (- (abs (/ n g))) (abs (/ n g)))
      (abs (/ d g)))))

(print-rat (make-rat 1 2))
(print-rat (make-rat -1 2))
(print-rat (make-rat 1 -2))
(print-rat (make-rat -1 -2))
