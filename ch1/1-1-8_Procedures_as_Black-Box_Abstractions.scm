;; 1.1.8 ブラックボックス抽象としての手続き

;; 平方根を計算する問題は，予測値が十分正確であるかどう判断するか，予測値をどう改善するのかなど
;; いくつかの部分問題に分けることができる

;; good-enough?手続きに関する限り，squareは手続きではなく手続き抽象（procedure abstraction）である．

;; 返す値を考えるなら，二乗を計算する次の手続きは区別できない
(define (square x) (* x x))

(define (square x)
  (exp (double (log x))))

(define (double x) (+ x x))

;; 局所名

(define (square x) (* x x))
(define (square y) (* y y))

;; 仮パラメタの違いは区別できない

;; 手続きのパラメタ名は手続き本体に対して局所的でなければならない

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;; good-enough?の仮引数xは，squareの仮引数xとは別物である．

;; 手続きの仮パラメタには，手続き定義の中で，仮パラメタがどんな名前を持っていても構わないという，特別な役目がある．
;; そういう名前を束縛変数（bound variable）といい，手続き定義は仮パラメタを束縛する（bind）という．

;; 変数が束縛されていないなら自由である．
;; 名前が束縛されている式の範囲を有効範囲という

;; 内部定義とブロック構造

;; sqrtで使う補助手続きをsqrtの中に隠す．
;; 他のgood-enough?やimproveと共存できる

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;; このような入れ子の構造をブロック構造という

