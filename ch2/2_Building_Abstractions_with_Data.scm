;; ch2. データによる抽象の構築

;; 2章では，データオブジェクトを組み合わせ，合成データをつくて抽象を構築する機能を扱う．

;; 線形結合 ax+byを作ることを考える
(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))

;; addやmulは+や*の基本的手続きではなく，引数a,b,x,yに渡したデータの種類に見合った演算を実行する
;; 必要なのは，linear-combinationがa,b,x,yについて知らなければならないのはaddやmulが適切な操作を実行することだけである．
;; linear-combination視点では，a,b,x,yがどんなデータ型なのか関心を持つ必要がない

;; データ抽象によりプログラムの部分部分で適切な抽象の壁（abstraction barrier）を建てることが可能なことを見る

;; 2.1 データ抽象入門
;; 2.1.1 有理数の算術演算
;; 有理数の演算をする．足し算，引き算，掛け算ができて，2つの有理数が等しいかのテストができるようにする

;; (make-rat <n> <d>) は分子が整数n，分母が整数dの有理数を返す
;; (numer <x>) 有理数xの分子を返す
;; (denom <x>) 有理数xの分母を返す

(load "./utils.scm")

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x ) (denom y))
     (* (numer y) (denom x))))

;; 対
(define x (cons 1 2))
(car x)
(cdr x)

(define y (cons 3 4))
(define z (cons x y))

(car (car z))
(car (cdr z))

;; 有理数の表現

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))

(print-rat one-half)

(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(print-rat (add-rat one-third one-third))

;; 2.1.2 抽象の壁

(define (make-rat n d)
  (cons n d))

(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

;; gcdを計算するタイミングを変えた
;; add-rat, sub-ratなどの手続きを変える必要は全くない

;; 表現の依存性を少数のインタフェース手続きに制限すると，
;; プログラムの設計に役立つだけでなく別の実装法を考える柔軟性を持ち続けることができる

;; 2.1.3 データとは何か

;; データとは，正しくは何なのか．
;; データは，選択子と構成子と，これらの手続きを有効な表現とするために満たすべき条件とで定義される．

;; 対の正体に触れずに，cons, car, cdrに触れた．
;; この３つの演算について知らなければならないのは，consでペアを作ってcarとcdrで値が取り出せることだけ

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

