;; 問題3.61 Sが定数項1のべき級数
;; S*X = 1になるXを探す

;;        S * X = 1
;; (1 + Sr) * X = 1
;;     X + Sr*X = 1
;;            X = 1 - Sr*x
(load "./ch3/exercise3-60.scm")

(define (invert-unit-series s)
  (cons-stream 1
               (stream-map - (mul-series (stream-cdr s) (invert-unit-series s)))))
(define x (invert-unit-series exp-series))
(define y (mul-series x exp-series))
(show-stream y 0 3)

