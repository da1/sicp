;; 問題1.38
;;1737年，スイスの数学者Leonhard EulerはDe Fractionibus Continuisというメモを発表した
;; 自然対数の底をeとしてe-2の連分数展開がある．
;; Niはすべて1
;; Diは1,2,1,1,4,1,1,6,1,1,8,...

;i=2,5,8,11,..
;  2 4 6 8
;3n+2
;(= (mod i 3) 2)
;2((/ i 3)+1)

;;cont-tracを使ってeを近似せよ

(load "./ch1/exercise1-37.scm")

(define (f i)
  (if (= (remainder (+ i 1) 3) 0)
    (* 2 (+ (quotient i 3) 1))
    1))

(map f '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22))

(+ 2 (cont-frac (lambda (i) 1.0) f 1000))

