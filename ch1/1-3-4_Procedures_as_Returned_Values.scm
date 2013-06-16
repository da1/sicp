;; 1.3.4 値として返される手続き
;; 引数に手続きが渡せると，言語の表現力が上がることがわかった．
;; 手続きを返す手続きが作れると，更に表現力が上がる．

(load "./ch1/1-3-3_Procedures_as_General_Methods.scm")
;; 平方緩和の考え方
(define (average-damp f)
  (lambda (x) (average x (f x))))

;; average-dampは手続きfを引数にとる．
;; "xを引数にとり，xと(f x)の平均値を返す手続き"を返す手続きを返す

((average-damp square) 10)

;; average-dampを使ったsqrt
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(sqrt 16)

;; xの立方根が関数y->x/y^2の不動点であることに注意すれば，平方根の手続きを立方根を得るものに一般化することができる

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(cube-root 27)

;; * Newton法

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x) (* x x x))
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(sqrt 16)

;; * 抽象と第一級手続き
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

;; 一般に，プログラム言語には，要素を扱う方法に色々制限がある．
;; ほとんど制限のない要素は，第一級（first-class）であるという．
;; 第一級要素の権利と特権は，
;; * 変数として名前が付けられる
;; * 手続きに引数として渡せる
;; * 手続きの結果として返される
;; * データ構造に組み込める
;; である．

