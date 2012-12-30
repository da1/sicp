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

;; 問題3.50
;; stream-mapの手続き
(load "./stream.scm")
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

;; 問題3.51
(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

;; 問題3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
			 seq))
(stream-ref y 7)
(display-stream z)
;; メモ化しないと結果が変わる
;; display-stream zのときに再度accumが呼ばれてsumの値が変わる


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

;; 問題3.53
;; プログラムを走らせずに，
(define s (cons-stream 1 (add-streams s s)))
;;で定義するストリームの要素を述べよ
;; 自分自身を足してる．2のべき乗ストリーム

(show-stream s 0 10)

;; 問題3.54
;; add-streamのように，２つのストリームの要素ごとの積を生じる手続きmul-streamsを定義せよ
;; integersストリームを使い，n番目の要素がn+1の階乗になるストリームの定義を完成させよ
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams factorials integers)))
(show-stream factorials 0 10)

;; 問題3.55
;; ストリームSをとり，その要素は S0, S0+S1, S0+S1+S2,...
;; となるようなストリームを返す手続きを定義せよ
(define (partial-sums s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s)))))
(define p (partial-sums integers))
(show-stream p 0 10)

;; 問題3.56
;; Sは１から始まる
;; (scale-stream S 2), (scale-stream S 3), (scale-stream S 5)の要素はSの要素

;; 2^n・3^m・5^rで表せる整数のことをハミング数と呼ぶらしいです。
;; ストリームをマージする手続き
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))
;; ハミング数のストリーム
(define S (cons-stream 1 (merge
			  (scale-stream S 2)
			  (merge
			   (scale-stream S 3)
			   (scale-stream S 5)))))

(show-stream S 0 10)
;; 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, 40, 45, 48, 50, 54, 60, ... (sequence A051037 in OEIS).
;; http://en.wikipedia.org/wiki/Hamming_numbers

;; 問題3.57
;; n 番目のfibsを計算するときの加算の実行回数を示せ

;; メモ化してあれば，n番目を計算するのに，n-1番目に１回足すだけ．n回で済む
;; メモ化なしだと，ストリームを作るたびに計算しなおすので大量に計算する必要がある．
(load "./stream.scm")
(define count 0)
(define (reset) (set! count 0))
(define (add x y) (begin (set! count (+ count 1)) (+ x y)))

(define (add-streams s1 s2)
  (stream-map add s1 s2))
(define fibs
  (cons-stream 0
	       (cons-stream 1
			    (add-streams (stream-cdr fibs)
					 fibs))))

(define (show-fib n)
    (reset)
    (display (stream-ref fibs n))
    (newline)
    (display count))

(show-fib 10)

;; 問題3.58
;; 次の手続きにより計算されるストリームを解釈せよ
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(define ex1 (expand 1 7 6))
(show-stream ex1 0 10)
(stream-ref ex1 0)
;; 1 4 2 8 5 7 1 4 ...

(define ex2 (expand 3 8 2))
(show-stream ex2 0 5)
(stream-ref ex2 0)
;; 3 7 5 0 0 ..

;; 割り算の筆算のように計算される
;; carに商，cdrにあまりを下におろして続きの計算をするイメージ

;; 問題3.59
;; 無限ストリームでべき級数を扱う
;; a. べき級数を表現するストリームを受け取り，積分（定数項を除く）した項の係数ストリームを返す
(define (integrate-series s)
  (stream-map / s integers))
;; s0/1, s1/2, s2/3, ...

;; b. expは積分しても定数項を除けば同じ
(define exp-series (cons-stream 1 (integrate-series exp-series)))
(show-stream exp-series 0 10)

;; sinとcosの級数を生成する方法を示せ
;; sinの微分がcos，cosの微分が-sinであることを使え
(define cosine-series
  (cons-stream 1 (integrate-series (stream-map - sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
(show-stream cosine-series 0 5)
(show-stream sine-series 0 5)


;; 問題3.60
;; 係数のストリームとして表現したべき級数で，乗算の手続きの定義を完成せよ
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams
		(stream-map
		 (lambda (x) (* (stream-car s1) x))
		 (stream-cdr s2))
		(mul-series (stream-cdr s1) s2))))
;; scale-stream
;; (sinx)^2 + (consx)^2 = 1によるテスト
(define square-sine-and-square-cosine
   (add-streams
  (mul-series sine-series sine-series)
  (mul-series cosine-series cosine-series)))

(show-stream square-sine-and-square-cosine 0 3)


;; 問題3.61 Sが定数項1のべき級数
;; S*X = 1になるXを探す

;;        S * X = 1
;; (1 + Sr) * X = 1
;;     X + Sr*X = 1
;;            X = 1 - Sr*x

(define (invert-unit-series s)
    (cons-stream 1
                 (stream-map - (mul-series (stream-cdr s) (invert-unit-series s)))))
(define x (invert-unit-series exp-series))
(define y (mul-series x exp-series))
(show-stream y 0 3)

;; 問題3.62
;; べき級数の割り算
;; これを使って，tanのべき級数を生成する方法を述べよ
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
    (error "zero division occured.")
    (mul-series s1 (invert-unit-series s2))))

(define tan (div-series sine-series cosine-series))
(show-stream tan 0 11)

;; tan xのべき級数
;; 0 1 0 1/3 0 2/15 0 17/315 0 62/2835 1382/155925


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

;; 問題3.63
(define (sqrt-stream x)
  (cons-stream 1.0
	       (stream-map (lambda (guess)
			     (sqrt-improve guess x))
			   (sqrt-stream x))))
;; ストリームを最初から構成することになって効率が悪い
;; メモ化しないなら一緒

;; 問題3.64
(define (stream-limit s q)
  (let ((s1 (stream-car s))
	(s2 (stream-car (stream-cdr s))))
    (if (< (abs (- s1 s2)) q)
	s2
	(stream-limit (stream-cdr s) q))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2.0 0.001)

;; 問題3.65
(define (log-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (log-summands (+ n 1)))))

(define log-stream
  (partial-sums (log-summands 1)))

(show-stream log-stream 0 10)
(show-stream (euler-transform log-stream) 0 10)
(show-stream (accelerate-sequence euler-transform log-stream) 0 10)

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

;; 問題3.66
(define p (pairs integers integers))
(show-stream p 0 10)
;; http://d.hatena.ne.jp/tmurata/20100302/1267486013

;; 問題3.67
;; http://www.serendip.ws/archives/1675
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (interleave
        (stream-map (lambda (x) (list (stream-car t) x))
                    (stream-cdr s))
        (stream-map (lambda (x) (list x (stream-car s)))
                    (stream-cdr t)))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define pairs-of-integers (pairs integers integers))
(show-stream pairs-of-integers 0 10)

;; 問題3.68
(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
	       t)
   (pairs (stream-cdr s) (stream-cdr t))))

;; http://www.serendip.ws/archives/1682
;; cons-streamを使ってないため無限ループになる

;; 問題 3.69
(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (cons (stream-car s) x))
                  (pairs (stream-cdr t) (stream-cdr u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define triples-of-integers (triples integers integers integers))
(show-stream triples-of-integers 0 1)

(define pythagoras
  (stream-filter (lambda (triple)
                         (= (+ (square (car triple))
			       (square (cadr triple)))
			    (square (caddr triple))))
                 triples-of-integers))
(show-stream pythagoras 0 5)

;; 問題3.70
(define (merge-weighted pairs1 pairs2 weight)
  (cond ((stream-null? (stream-car pairs1)) pairs2)
	((stream-null? (stream-car pairs2)) pairs1)
	(else
	 (let ((p1car (stream-car pairs1))
	       (p2car (stream-car pairs2)))
	   (if (< (weight p1car) (weight p2car))
	       (cons-stream p1car (merge-weighted pairs2 (stream-cdr pairs1) weight))
	       (cons-stream p2car (merge-weighted pairs1 (stream-cdr pairs2) weight)))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define (add-pairs-weight pair)
  (+ (car pair) (cadr pair)))

(define p (weighted-pairs integers integers add-pairs-weight))
(show-stream p 0 3)

(define (add-pairs-weight2 pair)
  (let ((i (car pair))
	(j (cadr pair)))
    (+ (* 2 i)
       (* 3 j)
       (* 5 i j))))

(define integers-no-remainder-2-3-5
  (stream-filter (lambda (x)
		   (not (and (= 0 (remainder x 2))
			     (= 0 (remainder x 3))
			     (= 0 (remainder x 5)))))
		 integers))

(define q (weighted-pairs integers-no-remainder-2-3-5 integers-no-remainder-2-3-5 add-pairs-weight2))
(show-stream q 0 3)

;; 問題3.71
(define (cube x) (* x x x))
(define (cube-weight x)
  (let ((i (car x))
	(j (cadr x)))
    (+ (cube i) (cube j))))
(define cube-weighted-stream
  (weighted-pairs integers integers cube-weight))

(define (ramanujan-sub s)
  (let ((s-weight (cube-weight (stream-car s)))
	(t-weight (cube-weight (stream-car (stream-cdr s)))))
    (if (= s-weight t-weight)
	(cons-stream s-weight
		     (ramanujan-sub (stream-cdr s)))
	(ramanujan-sub (stream-cdr s)))))

(define ramanujan-stream
  (ramanujan-sub cube-weighted-stream))

(show-stream ramanujan-stream 0 5)


;; 問題3.72
(define (square-weight x)
  (let ((i (car x))
	(j (cadr x)))
    (+ (square i) (square j))))
(define square-weighted-stream
  (weighted-pairs integers integers square-weight))
(define (sum-of-two-square-sub s)
  (let ((s-weight (square-weight (stream-car s)))
	(t-weight (square-weight (stream-car (stream-cdr s))))
	(u-weight (square-weight (stream-car (stream-cdr (stream-cdr s))))))
    (if (= s-weight t-weight u-weight)
	(cons-stream s-weight
		     (sum-of-two-square-sub (stream-cdr s)))
	(sum-of-two-square-sub (stream-cdr s)))))
(define sum-of-two-square-three-way
  (sum-of-two-square-sub square-weighted-stream))

(show-stream sum-of-two-square-three-way 0 5)

;; 信号としてのストリーム
;; 積分機
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
		 (add-streams (scale-stream integrand dt)
			      int)))
  int)

;; 問題3.73
(define (RC R C dt)
  (define (rc i-st v-zero)
    (add-streams
      (scale-stream i-st R)
      (integral (scale-stream i-st (/ 1 C))
                v-zero
                dt)))
  rc)

(define RC1 (RC 5 1 0.5))
(show-stream (RC1 ones 0) 0 10)

;; 問題3.74
(define sense-data
  (stream-map (lambda (x) (sin x)) integers))

(define (sign-change-detector s2 s1)
  (cond ((and (<= 0 s2) (> 0 s1)) 1)    ;; 負->正 1
        ((and (> 0 s2) (<= 0 s1)) -1)   ;; 正->負 -1
        (else 0)))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
			(stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

(define zero-crossings
  (stream-map sign-change-detector sense-data
	      (cons-stream 0 sense-data)))

(show-stream zero-crossings 0 10)

;; 問題3.75
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
		 (make-zero-crossings (stream-cdr input-stream)
				      (stream-car input-stream) avpt))))

(define zero-crossings (make-zero-crossings sense-data 0 0))
(show-stream zero-crossings 0 10)

;; 問題3.76
(define (smooth st)
  (stream-map (lambda (x y) (/ (+ x y) 2.0)) st (stream-cdr st)))

(define zero-crossings (make-zero-crossings (smooth sense-data) 0))
(show-stream zero-crossings 0 10)

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

;; 問題3.77
;; 3.5.2のintegers-starting-fromによく似たintegralの定義

;; 問題3.78
;; 図3.35のダイアグラムをschemeにする

;; 問題3.79
;; 3.78を一般化して、一般的な二階微分方程式を解けるようにせよ

;; 問題3.80
(define (RLC R L C dt)
  (define (rlc vC0 iL0)
    (define dvC (scale-stream iL (/ -1 C)))
    (define diL (add-streams (scale-stream vC (/ 1 L))
			     (scale-stream iL (- (/ R L)))))
    (define iL (integral (delay diL) iL0 dt))
    (define vC (integral (delay dvC) vC0 dt))
;;    (stream-map (lambda (v i) (cons v i)) vC iL))
    (stream-map cons vC iL))
  rlc)

(define RLC1 (RLC 1 1 0.2 0.1))
(show-stream (RLC1 10 0) 0 10)

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

;; 問題3.81
(define (rand-update x)
  (remainder (+ x 1812433253) 4294967296))
(define (rand input-stream random-init)
  (define random-stream
    (if (stream-null? input-stream)
	the-empty-stream
	(let ((request (stream-car input-stream)))
	  (cons-stream
	   (cond ((eq? request 'generate) (rand-update random-init))
		 ((number? request) (rand-update request))
		 (else (error "Unknown request -- RAND" request)))
	   (rand (stream-cdr input-stream) (stream-car random-stream))))))
  random-stream)

(define request-stream
  (cons-stream 100
	       (cons-stream 'generate
			    (cons-stream 'generate
					 (cons-stream 100
						      (cons-stream 'generate
								   (cons-stream 'generate the-empty-stream)))))))
(show-stream (rand request-stream 1) 0 6)
;;何か正しく動いてない

;; 問題3.82 ;;***要再チェック
(define (estimate-integral p x1 x2 y1 y2)
  (define monte-carlo-stream
    (stream-map (lambda (m)
		  (* (- x2 x1) (- y2 y1) m))
		(monte-carlo random-in-range-experiment-stream 0.0 0.0)))
  (define random-in-range-experiment-stream
    (stream-map p random-in-range-x-stream random-in-range-y-stream))
  (define random-in-range-x-stream
    (stream-map (lambda (x) (random-in-range x1 x2)) ones))
  (define random-in-range-y-stream
    (stream-map (lambda (x) (random-in-range y1 y2)) ones))
  monte-carlo-stream)

;;;; よりスマートな estimate-integral
(define (estimate-integral p x1 x2 y1 y2)
  (stream-map (lambda (m) (* (- x2 x1) (- y2 y1) m))
	      (monte-carlo (stream-map p
				       (stream-map
					(lambda (x) (random-in-range x1 x2)) ones)
				       (stream-map
					(lambda (x) (random-in-range y1 y2)) ones))
			   0.0 0.0)))

;; 面積と面積から算出した円周率piを表示する手続き
(define (pi-from-monte-carlo-simulation circle-area radius)
  (display circle-area)
  (newline)
  (/ circle-area radius))

; 中心(5, 5) 半径5 の円の場合
;テスト手続き
(define (p-test x y)
  (<= (+ (square (- x 5)) (square (- y 5))) (square 5)))
(use srfi-27)
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random-real) range))))

;;結果
(pi-from-monte-carlo-simulation
 (stream-ref (estimate-integral p-test 0 10 0 10) 100000) (* 5 5))

;gosh> 79.02120978790212
;3.160848391516085

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

