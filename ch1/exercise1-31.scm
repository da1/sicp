;; 問題1.31
;;a. sum手続きは高階手続きとして書ける抽象の中でも最も単純なものに過ぎない

;; 与えられた範囲の点での関数値の積を返すproductという似た手続きを書け．
;; productを使ってfactorialを定義せよ
;; πの近似値を計算せよ

(load "./ch1/1-3_Formulating_Abstractings_with_Higher-Order_Procedures.scm")
(load "./utils.scm")

(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(factorial 0)
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)

(define (pi-product n)
  (define (pi-next x) (+ x 1))
  (define (pi-term x)
    (if (even? x)
      (/ (+ x 2) (+ x 1))
      (/ (+ x 1) (+ x 2))))
  (product pi-term 1 pi-next n))

(* 4.0 (pi-product 10))
(* 4.0 (pi-product 100))
(* 4.0 (pi-product 1000))

;; b. 反復プロセス版

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))
