;; 問題2.10
;; ゼロをまたがる区間
(load "./ch2/2_Building_Abstractions_with_Data.scm")

(define r (div-interval
  (make-interval 1 1)
  (make-interval -1 1)))

(lower-bound r)
(upper-bound r)

(define (div-interval x y)
  (if (< (* (lower-bound y) (upper-bound y)) 0)
    (error "error")
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))
