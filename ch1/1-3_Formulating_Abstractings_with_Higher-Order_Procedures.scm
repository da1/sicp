;; 1.3 高階手続きによる抽象

(define (cube x) (* x x x))

;; 手続きを値として返す手続きが構成したくなる．
;; このような手続きを高階手続ききという．

;; 1.3.1 引数としての手続き
;; aからbまでの整数の和を計算する

(define (sum-integers a b)
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))

;;与えられた範囲の整数の3乗の和を計算する
(define (sum-cubes a b)
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (+ a 1) b))))

;; 級数の項の並びの和を計算する
(define (pi-sum a b)
  0
  (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b)))

;; 3つの手続きは明らかに共通の基本パターンを持っている．

;; 我々の手続き言語では，ひな形にスロットを仮パラメタとして与えることで，総和自身の概念を表現する．
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (inc a) (+ a 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))

;; sumを定義すると，更にこれを使って新しい概念の表現部品になる

;; 定積分を手続きとして表現する

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)
