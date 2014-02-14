;; 3.5 ストリーム
;; 状態をモデル化する，ストリームというデータ構造

;; 遅延評価（delayed evaluation）の技法を取り入れる
;; ストリーム処理は，代入や可変データを使わずに状態を持つシステムをモデル化させる

;; 3.5.1 ストリームは遅延リスト
;; ストリームは並びをリストとして操作するコストを追うことなく，並びの操作を使わせる懸命な方法である

;; 表面的には，ストリームはそれを操作する手続きに，異なる名前が付いているリストである
(stream-car (cons-stream x y)) = x
(stream-cdr (cons-stream x y)) = y

;; リスト演算のストリーム版
(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))

;; ストリームの実装のためには，cons-streamでストリームが構成されたときではなく，stream-cdrでアクセスされた時にストリームのcdrが評価されるようにする

;; ストリームの実装にはdelayという特殊形式を使う
;; (delay <exp>)の評価は式を評価せずに，遅延オブジェクト（delayed object）を返す

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;; ストリーム実装の働き
(stream-car
  (stream-cdr
    (stream-filter prime?
                   (stream-enumerate-interval 10000 1000000))))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))

;; cons-streamで作られたstream-enumerate-intervalの返す値は
;; (cons 10000 (delay (stream-enumerate-interval 10001 1000000)))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;; delayとforceの実装
(define (forcce delayed-object)
  (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
        (begin (set! result (proc))
               (set! already-run? #t)
               result)
        result))))

;; delayの定義
;; (memo-proc (lambda () <exp>))
(define-macro (delay x) `(memo-proc (lambda () ,x)))
; (define-macro (delay x) `(lambda () ,x))
(define (force x) (x))

;; 3.5.2 無限ストリーム
;; 無限の長さをもったストリームを扱う
(load "./stream.scm")
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(stream-car integers)
(stream-cdr integers)

;; 7で割り切れない整数のStream
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7))) integers))

(stream-ref no-sevens 100)

;; フィボナッチ数の無限ストリーム
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
;; (0, fib(1,1))
;; (0, (1, fib(1,2)))
;; (0, (1, (1, fib(2, 3))))
;; (0, (1, (1, (2, fib(3, 5)))))


;; no-sevensを一般化する
;; エラトステネスの篩 素数の無限ストリーム
(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 50)

;; ストリームの暗黙の定義
;; 無限ストリームを暗黙に定義する
;; 1の無限ストリーム
(define ones (cons-stream 1 ones))
;; ２つのストリームの和

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))
(stream-ref integers 10)

;; フィボナッチ数
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (show-stream stream n m)
  (if (< n m)
    (begin
      (display (stream-ref stream n))
      (newline)
      (show-stream stream (+ n 1) m))
    (stream-ref stream n)))
(show-stream fibs 0 5)

;; 2のべき乗のストリーム
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define double (cons-stream 1 (scale-stream double 2)))
(show-stream double 0 10)

;; 素数
(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))
(define (square x) (* x x))
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes))
;; psのリストから順に素数を取り出していって，nが素数で割り切れたらfalse
;; 素数がroot nより大きくなったらおしまい．trueを返す．
(show-stream primes 0 10)

;;3.5.3 ストリームパラダイムの開発
;; 反復をストリームプロセスとして形式化する
(load "./stream.scm")
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses))))
;; ex1-7 average
(show-stream (sqrt-stream 2) 0 10)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
(show-stream pi-stream 0 10)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(show-stream (euler-transform pi-stream) 0 10)

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerate-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))
(show-stream (accelerate-sequence euler-transform pi-stream) 0 10)




;; 対の無限ストリーム
(stream-filter (lambda (pair)
                 (prime? (+ (car pair) (cadr pair))))
               int-pairs)

(define (stream-append s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (stream-append (stream-cdr s1) s2))))

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

;; 信号としてのストリーム
;; 積分機
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

;; 3.5.4 ストリームと遅延評価
;; delayを使ったcons-streamではうまく表現できない例
(define int
  (cons-stream initial-value
               (add-streams (scale-stream integrand dt)
                            int)))

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)

;; 遅延引数
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
;; 昔のgoshのバージョン（0.8）ではfとyを逆にしないと動かなかった

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

;; 正規順序の評価
;; 3.5.5 関数的プログラムの部品化度とオブジェクトの部品化度（モジュラリティ）
;; ストリーム処理の観点から、モンテカルロ法によるπの見積り（3.1.2節）
(define random-init 7)
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
    (f (stream-car s) (stream-car (stream-cdr s)))
    (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

;; cesaro-streamはmonte-carlo手続きに与えられ、それは確率の見積りのストリームを作り出す
;; 次にその結果は、πの見積りのストリームへと変換される。
;; プログラムのこの版は、試行を何回行うかを示すパラメータはもはや必要としない
;; piストリームを先のほうまで眺めると、πのよりよい見積りが得られる
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (average x y)
  (/ (+ x y) 2))

;; 時の関数型プログラミング的視点
;; 支払い処理機の実装を考え直す
;; 3.1.3節ででてきたもの
(define (mak-esimplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define (stream-withdraw balance amount-stream)
  (cons-stream
    balance
    (stream-withdraw (- balance (stream-car amount-stream))
                     (stream-cdr amount-stream))))

;;stream-withdrawは出力が入力によって完全に決定される
;;入力amount-streamが、利用者が入力した順次の値のストリームであり、結果としての
;;残高のストリームが表示されているとしよう。
;;値を入力し、結果を眺めている利用者の見え方からは、ストリーム処理は、make-simplified-withdrawで作り出したオブジェクトと同じ振る舞いをする。
;; ところが、ストリームには代入も局所状態も存在しない。したがって、3.1.3節で直面した理論的困難は存在しない。しかもシステムには状態が存在する
;;システムに状態を持たせるのは利用者の時間的存在である

;;本章は、モデル化しようとする実世界の認識に合致するモデルを持つ、計算モデルを構築するという
;;目的を持って始めた。
;;われわれは、バラバラで、時に縛られ相互作用する状態を持つオブジェクトの集まりで世界をモデル化することもできるし、また、単一の時にしばられない状態のない個体で世界をモデル化することもできる
;;どちらの見方も強力な利点があるが、どちらかだけでは完全に満足できない。もっと素晴らしい統合が現れなければならない

