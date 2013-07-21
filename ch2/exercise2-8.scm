;; 問題2.8 2つの区間の差の計算法を書け
(load "./ch2/exercise2-7.scm")

;; x - y
;; 最大値 xの最大-yの最小
;; 最小値 xの最小-yの最大

(define (sub-interval x y)
  (let ((lower (- (lower-bound x) (upper-bound y)))
        (upper (- (upper-bound x) (lower-bound y))))
    (make-interval lower upper)))

(define a
  (sub-interval
    (make-interval 0 1)
    (make-interval 0 1)))

(lower-bound a)
(upper-bound a)
