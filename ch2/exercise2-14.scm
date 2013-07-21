;; 問題2.14
;; Lemの正しいことを示せ

;; par2は，常に相対許容誤差0のoneとの掛け算を計算しているので近似なし，真の値と同じ結果
;; par1は(mul-interval r1 r2)で近似をしているので真の値との誤差が生じる

(load "./ch2/exercise2-12.scm")

(define a (make-interval 99.0 101.0))
(define b (make-interval 49.5 50.5))
(define one (make-interval 1 1))

(define x (div-interval a a))
(lower-bound x)
(upper-bound x)

(define (mul-interval-appr x y)
  (make-center-percent
    (* (center x) (center y))
    (+ (percent x) (percent y))))

(define (div-interval-appr x y)
  (mul-interval-appr x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (show x)
  (print (lower-bound x))
  (print (upper-bound x)))

(show (div-interval a a))
(show (div-interval-appr a a))

(show (div-interval a b))
(show (div-interval-appr a b))

(show (div-interval one a))
(show (div-interval-appr one a))
