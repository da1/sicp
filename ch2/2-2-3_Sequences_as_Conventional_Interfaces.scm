;; 2.2.3 公認インタフェースとしての並び

;; データ構造を扱う上のもう一つの強力な設計原理，公認インタフェース（conventional interfaces）の利用を説明する

;; 2.2.2の，count-leaves手続きに類似の，木を引数にとり奇数の葉の二乗の和を計算する

(load "./utils.scm")

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define t (list 1 (list 2 (list 3 4)) 5))
(sum-odd-squares t)

;; 与えられた整数nより小さいか等しいkに対して，偶数のFibonacci数Fib(k)のリストを作る手続きとは，一見して非常に違っている

(define (even-fibs n)
  (define (next k)
    (if (> k n)
      nil
      (let ((f (fib k)))
        (if (even? f)
          (cons f (next (+ k 1)))
          (next (+ k 1))))))
  (next 0))

(even-fibs 10)

;; はじめのプログラムは
; * 木の葉を数え上げる
; * フィルタを通して奇数を選ぶ
; * 選ばれたものを二乗する
; * ()を初期値に，結果を+してアキュムレートする

;; 次のプログラムは
; * 整数を0からnまで数え上げる
; * 整数のFibonacci数を計算する
; * フィルタを通して偶数を選ぶ
; * 空リストの初期値に結果をconsしてアキュムレートする

;; 信号処理の構造が明確になるようにプログラムを構築しなおす

;; 並びの演算
(map square (list 1 2 3 4 5))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons nil (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(sum-odd-squares t)

(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))


;; 与えられた整数nより小さいか等しいkに対して，偶数のFibonacci数Fib(k)のリストを作る手続きとは，一見して非常に違っている

(define (even-fibs n)
  (define (next k)
    (if (> k n)
      nil
      (let ((f (fib k)))
        (if (even? f)
          (cons f (next (+ k 1)))
          (next (+ k 1))))))
  (next 0))

(even-fibs 10)

;; はじめのプログラムは
; * 木の葉を数え上げる
; * フィルタを通して奇数を選ぶ
; * 選ばれたものを二乗する
; * ()を初期値に，結果を+してアキュムレートする

;; 次のプログラムは
; * 整数を0からnまで数え上げる
; * 整数のFibonacci数を計算する
; * フィルタを通して偶数を選ぶ
; * 空リストの初期値に結果をconsしてアキュムレートする

;; 信号処理の構造が明確になるようにプログラムを構築しなおす

;; 並びの演算
(map square (list 1 2 3 4 5))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons nil (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(sum-odd-squares t)

(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(even-fibs 10)

;; 最初のn+1個のFibonacci数の二乗のリストを作る
(define (list-fib-squares n)
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

(list-fib-squares 10)

; 並びの中野奇数の二乗の積の計算にも使える
(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))

; 個人レコードの並びがあり，最高収入のプログラマの給料を見つけたい
;(define (salary-of-highest-paid-programmer records)
;  (accumulate max
;              0
;              (map salary
;                   (filter programmer? records))))

; リストとして実装した並びは，処理部分を組み合わせるのを可能にする公認インタフェースとして役立つ
; プログラムのデータ構造を少数の並びの演算の中に局所化することができる
