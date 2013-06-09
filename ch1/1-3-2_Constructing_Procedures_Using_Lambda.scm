;; 1.3.2 lambdaを使う手続きの構築

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(define (plus4 x) (+ x 4))
;; と
(define plus4 (lambda (x) (+ x 4)))
;; は等価

;; * 局所変数を作り出すletの使い方

;;f(x,y) = x(1+xy)^2+y(1-y)+(1+xy)(1-y)
;;を計算したい

;; a = 1+xy
;; b = 1-y
;; とおいて，
;; f(x,y) = xa^2+yb+ab
;;と書ける

;; これをlambdaを使って実現する

(define (f x y)
  ((lambda (a b)
     (+ (x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

;; 特殊形式letを使うと
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
     (+ (x (square a))
        (* y b)
        (* a b))))

;; 一般的に
(let ((<var1> <exp1>)
      (<var2> <exp2>)
      ...
      (<varn> <expn>))
  <body>)

;;これは
((lambda (<var1> .. <varn>)
   <body>)
 <exp1>
 ...
 <expn>)
;;と解釈される
;; letはlambdaのシンタックスシュガーである


