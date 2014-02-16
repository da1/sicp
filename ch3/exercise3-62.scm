;; 問題3.62
;; べき級数の割り算
;; これを使って，tanのべき級数を生成する方法を述べよ
(load "./ch3/exercise3-61.scm")
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
    (error "zero division occured.")
    (mul-series s1 (invert-unit-series s2))))

(define tan (div-series sine-series cosine-series))
(show-stream tan 0 11)

;; tan xのべき級数
;; 0 1 0 1/3 0 2/15 0 17/315 0 62/2835 1382/155925

