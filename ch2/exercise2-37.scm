;; 問題2.37'

; ベクタv=(vi)を数の並びで，マトリクスm=(mij)をベクタの並びで表現するとしよう

(load "./ch2/exercise2-36.scm")

(define v (list
            (list 1 2 3 4)
            (list 4 5 6 6)
            (list 6 7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (list 1 2 3 4) (list 1 2 3 4))

(define (matrix-*-vector m v)
  (map (lambda (x) (* x v)) m))

(matrix-*-vector (list 1 2 3 4) 10)

(define (transpose mat)
  (accumulate-n
    (lambda (x y) (append (list x) y))
    nil mat))

(transpose v)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mr)
           (map (lambda (col) (dot-product mr col)) cols))
      m)))

(matrix-*-matrix
  (list (list 1 0 0))
  (list (list 1) (list 1) (list 1)))

(matrix-*-matrix
  (list (list 1 0) (list 0 1))
  (list (list 1 0) (list 0 1)))
