;3章 標準部品
;3.1 代入と局所状態
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))

;
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(W1 50)
(W2 70)
(W2 40)
(W1 40)

;
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
	  balance))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT"
		       m))))
dispatch)

(define acc (make-account 100))
((acc 'withdraw) 50)

((acc 'withdraw) 60)
((acc 'deposit) 40)
((acc 'withdraw) 60)

(define acc2 (make-account 100))

;;問題3.1
(define (make-accumulator sum)
  (lambda (x)
    (begin (set! sum (+ sum x))
	   sum)))

(define A (make-accumulator 5))
(A 10)
;15
(A 10)
;25

;問題3.2
(define (make-monitored f)
  (define count 0)
  (define (reset-count)
    (begin (set! count 0)
	   count))
  (define (dispatch mf)
    (cond ((eq? mf 'reset-count) (reset-count))
	  ((eq? mf 'how-many-calls?) count)
	  (else (begin
		  (set! count (+ count 1))
		  (f mf)))))
dispatch)


(define s (make-monitored sqrt))
(s 100)
;10
(s 'how-many-calls?)
;1

(define s2 (make-monitored sqrt))
(s2 9)
(s2 25)
(s2 'how-many-calls?)
(s2 'reset-count)
(s2 'how-many-calls?)

;問題3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
	  balance))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT"
		       m))))
  (define (authenticate p m)
    (if (eq? password p)
	(dispatch m)
	(error "Incorrect Password")))
authenticate)


(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
;60
((acc 'some-other-password 'diposit) 50)
;Incorrect password

;問題3.4
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
	  balance))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT"
		       m))))
  (define (authenticate p m)
    (if (eq? password p)
	(dispatch m)
	(if (>= (auth 'how-many-calls?) 6)
	    call-the-cops
	    auth)))
  (define (Incorrect x)
    "Incorrect password")
  (define auth (make-monitored Incorrect))
  (define (call-the-cops x)
    "call-the-cops")
authenticate)

;3.1.2 代入を取り入れた利点
;乱数生成について
;http://sicp.g.hatena.ne.jp/hyuki/20060505/random
(define (rand-update x)
  (define A 1103515245)
  (define B 12345)
  (define M 2147483647)
  (mod (+ (* A x) B) M))

(define random-init 8)

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))
(rand)

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
(iter trials 0))
(estimate-pi 10000)

(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
	(cond ((= trials-remaining 0)
	       (/ trials-passed trials))
	      ((= (gcd x1 x2) 1)
	       (iter (- trials-remaining 1)
		     (+ trials-passed 1)
		     x2))
	      (else
	       (iter (- trials-remaining 1)
		     trials-passed
		     x2))))))
(iter trials 0 initial-x))

;問題3.5
;モンテカルロ積分
(use srfi-27)
;(load "/usr/share/slib/random.scm")
;(define (random-in-range low high)
;  (let ((range (- high low)))
;    (+ low (random range))))
;(random-in-range 0.0 1.0)
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random-real) range))))
(random-in-range 0.0 1.0)

(define (P px py r)
  (define (square value)
    (* value value))
  (lambda (x y)
    (>= (square r) 
     (+ (square (- x px)) (square (- y py))))))
(define p1 (P 0 0 1))
(p1 0 0)
(p1 1 0)
(p1 1 1)

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (p-test)
    (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (* (- x1 x2) (- y1 y2))
     (monte-carlo trials p-test)))
(estimate-integral (P 0.0 0.0 1.0) 1.0 -1.0 1.0 -1.0 100000)

;問題3.6
(define rand
  (let ((x 11))
    (define (reset new-value)
      (set! x new-value) x)
    (define (generate)
      (set! x (rand-update x)) x)
    (define (dispatch m)
      (cond ((eq? m 'reset) reset)
	    ((eq? m 'generate) (generate))
	    (else (error "Unknown request -- RAND" m))))
    dispatch))

(rand 'generate)
((rand 'reset) 101)
((rand 'reset) 11)

;==========
;3.1.3 代入を取り入れた代償
;; set!により、局所状態を持ったオブジェクトがモデル化できた。
;; ただし、その代償として、手続きの置き換えモデルを使った解釈を失った。
;; また、参照透明であることも失い、オブジェクトが同一かそうでないかの判断が難しくなった。

;2章でやったような代入を伴わないプログラミングは関数型プログラミングという

;代入が話を難しくしている例 3.1.1のmake-withdraw手続きの簡易版
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define W (make-simplified-withdraw 25))
(W 20)
(W 10)

;setを使わないmake-decrementer手続き（をmake-withdrawと比べる）
(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))
(define D (make-decrementer 25))
(D 20)
;; 5
(D 10)
;; 15

;置き換えモデルを使ったmake-decrementerの働きの説明
((make-decrementer 25) 20)
((lambda (amount) (- 25 amount)) 20)
(- 25 20)

;同様にmake-simpified-withdrawを置き換えモデルにしてみる
((make-simplified-withdraw 25) 20)
;make-simplified-withdraw中のbalanceを25に置き換える
((lambda (amount) (set! balance (- 25 amount)) 25) 20)
;lambda式中のamountを20で置き換える
(set! balance (- 25 20)) 25
;balanceに5をセットしたあとに25を返す?

;今までの考え方では、変数とは値に対する名前であった。
;変数の値は変わりうる、という考えを取り入れると変数は単なる値の名前ではなくなる。
;変数は値が格納される場所を指す。その場所に格納される値は変わりうる。

;==========
;同一と変化
;; 2つのものは「同じ」であるということについて考える

(define D1 (make-decrementer 25))
(define D2 (make-decrementer 25))
;D1とD2は同じ

;; W1とW2は同じ？
(define W1 (make-simplified-withdraw 25))
(define W2 (make-simplified-withdraw 25))
(W1 20)
(W1 20)
(W2 20)

;任意の式でW1をW2で置き換えることはできない
;(W1 20)と(W2 20)で結果が違う。
;W1とW2は別物である

;"式の評価結果を変えずに、式中のものをそれと等しいもので置き換えることができる"言語は
;参照透明（referentially transparent）である
;他のスレッドが値を書き換えることがある(3.4
;参照透明を捨てたとき、"同じ"とはどういう意味なのだろうか
;同じように見える2つのオブジェクトについて、一方を変えてみてもう一方も同じように変わるかを見て決める。
;変化を観測しないことには同一性を確かめることはできない

;p130
(define peter-acc (make-account 100))
(define paul-acc (make-account 100))
((paul-acc 'withdraw) 60)
((peter-acc 'withdraw) 60)

(define peter-acc (make-account 100))
(define paul-acc peter-acc)
((paul-acc 'withdraw) 60)
((peter-acc 'withdraw) 60)

;上の例ではpeter-accとpaul-accは別もの
;下の例では両者は同じもの
;このような状況は計算モデルの構築に混乱をもたらす。
;paul-accが変えられている場所を探すには、peter-accが変えられている場所も探さなくてはいけない

;オブジェクトに状態の概念を取り入れることで、話は複雑になる。
;払い出しをして残高が変わる同じ銀行口座
;同じ状態の異なる2つの銀行口座

;==========
;命令形プログラムの落とし穴
;代入を多用するプログラミングは命令形プログラミングという
;計算モデルの複雑性を高める上に、命令形の流儀で書いたプログラムには、関数型プログラムには起こりえぬ虫を入れ易い

;1.2.1節の反復的な階乗プログラム
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
	product
	(iter (* counter product)
	      (+ counter 1))))
  (iter 1 1))
(factorial 5)

;代入を使って書きなおしたfactorial
(define (factorial n)
  (let ((product 1)
	(counter 1))
    (define (iter)
      (if (> counter n)
	  product
	  (begin (set! counter (+ counter 1))
		 (set! product (* counter product))
;	  (begin (set! product (* counter product))
;		 (set! counter (+ counter 1))
		 (iter))))
    (iter)))
(factorial 5)

;うっかり順番を間違えると大変なことになる
;こういうことは関数型プログラミングにはない
;「この変数の設定をあの前にすべきか、あとにすべきか」という問題は、初心者プログラマの負担になっている

;複数のプロセスが並列に走る状況を考えるともっと大変なことになる（3.4節）

;問題3.7
;問題3.3で考えてたパスワード付きのmake-account
;共同口座を作る機能make-joint
;引数 パスワードで保護された口座 パスワード 新しいパスワード

(define (make-joint account account-password new-password)
  (define (dispatch password m)
    (if (eq? password new-password)
	(account account-password m)
	(error "Incorrect password account-password")))
  dispatch)

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
	  balance))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT"
		       m))))
  (define (authenticate p m)
    (if (eq? password p)
	(dispatch m)
	(error "Incorrect Password")))
authenticate)

(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 40)
;60
((peter-acc 'some-other-password 'diposit) 50)
;Incorrect password

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'open-sesame 'withdraw) 60)
((paul-acc 'rosebud 'withdraw) 50)
((peter-acc 'rosebud 'withdraw) 10)
;make-joinしたときにパスワードが違ってると動かない
;パスワード変えられない

;問題3.8
;(+ (f 0) (f 1))が、+の引数を左から右へ評価すると0を返し、右から左へ評価すると1を返すようなf
(define f
  (let ((x 1))
    (lambda (n)
      (if (= n 0)
	  (begin (set! x 0) x)
	  x))))
;引数に0以外を渡すと、内部変数（初期値1）の値をそのまま返す。
;0を渡すと内部変数を0にしてから、内部変数の値を返す
(f 1)
(f 0)
(f 1)

(+ (f 0) (f 1))
(+ (f 1) (f 0))

;==========
;3.2 評価の環境モデル
;今までの置き換えモデルは、代入が存在する状況では使えない。
;評価の新しいモデル
;変数は環境（environments）という構造の中に確保される。
;環境とはフレーム（frames）の並びである。
;各フレームは束縛（binding）の表である。表は空であることもある。
;　束縛によって変数名とその値を対応付ける
;各フレームは、外側の環境（enclosing environment）へのポインタを持つ。
;　ただし、大域（global）であるものを除く。
;ある環境での、変数の値（value of a variable）は、その変数の束縛をもつ最初のフレームでの変数の束縛で得られる値である。

;図3.1
;環境Dでのxの値は3
;環境Bでのxの値は3。Bにxの束縛がないため、その環境Dを見に行く。Dで束縛が見つかる。
;環境Aでのxの値は7。環境Aに関して、フレームIIのxの7への束縛は、フレームIのxの3への束縛を隠すという。

;式はそれ単体では意味をもたず、環境の中で評価される。
;+記号は、大域環境の中で基本加算手続きに束縛している。

;3.2.1 評価の規則
;1. 組み合わせの部分式を評価する
;2. 演算子部分式の式の値を、非演算子部分式の値に作用させる。

;例
(define (square x)
  (* x x))
(define square
  (lambda (x) (* x x)))
;(lambda (x) (* x x))を評価し、squareに束縛する
;defineは、フレームに束縛を追加する。

;手続きに引数を作用させる場合
;手続きを作用させると新しい環境ができる（図3.3 E1）。
;この環境の最初のフレーム内で手続きの仮パラメータが引数の値に束縛される。
;E1の環境内で手続きの本体を評価する

;手続きオブジェクトを、一組の引数に作用させるには、フレームを構成し、手続きの仮パラメータをその呼び出しの引数に束縛し
;今構成した新しい環境の文脈の中で、手続きの本体を評価する。
;新しいフレームは、外側の環境として作用させている手続きの環境部分を持つ。

;手続きを、与えられた環境に対してlambda式を評価して作り出す。
;結果の手続きオブジェクトは、lambda式の本文と手続きが作り出されたときの環境へのポインタである。
;defineを使った記号の定義は、現在の環境フレームで束縛を作り出し、値をその記号に割り当てる。
;既に現在のフレーム内で、変数に対する束縛があるならば、束縛が変更になる。

;set!が評価されるときの振る舞い
;その環境で、その変数の束縛を探し、束縛を変えて新しい値を表すようにする

;これらの評価規則は、抽象的であるが、解釈系が式を評価する方法の正しい記述を提供している。
;4章で、このモデルが実用の解釈系を実装する青写真として役立つことがわかる

;3.2.2 単純な手続きの作用
;図3.4
;square, sum-of-squares, fの3つの手続きの例
;(f 5)の評価

;fの呼び出しで新しい環境E1が作られる。仮パラメータaに5が束縛される
;sum-of-squaresの呼び出しで新しい環境E2が作られる。仮パラメータxとyが6と10に束縛される
;squareの呼び出しで新しい環境E3が作られる。仮パラメータxが6に束縛される。
;もうひとつのsquareも同様にE4を作って、xに10を束縛する

;別々のフレームにすることで、同じxという名前の複数の局所変数を分けておくことができる。

;部分式が評価されたら、結果が戻る。squareの呼び出しで生成された値がsum-of-squaresで足され、
;その結果がfにより返される。

;問題3.9
;1.2.1節の階乗を計算する手続き
;再帰版
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
(factorial 6)

;反復版
(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
		 (+ counter 1)
		 max-count)))
(factorial 6)
;これらを評価するときの環境構造を示せ


;3.2.3 局所変数の入れ物としてのフレーム
;局所状態を持つオブジェクトを表現するのに、手続きや代入がどう使えるのかを見る

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds")))

(define W1 (make-withdraw 100))
(W1 50)

;について考える
;図3.7 (define W1 (make-withdraw 100))評価の結果
;新しい環境E1が作られて、balanceに100が束縛される
;make-withdrawの本体であるlambda式を評価して、新しい手続きオブジェクトが作られる。
;defineしているため、W1がその手続きオブジェクトを指している

;図3.8 (W1 50)を評価したとき
;新しいフレームが作られて、仮パラメータamountに50が束縛される。
;このフレームは、外側の環境として、E1を指す。
;この中で手続きの本体を評価する。
;amountは自フレーム内、balanceは自分の一つ外側の環境E1内で見つかる。
;set!が実行されると、E1でのbalanceの束縛が変わる。
;E1は手続きオブジェクトW1の局所状態変数を保持する場として機能している。
;図3.9　W1呼び出し後の状況

(define W2 (make-withdraw 100))
;新しい手続きオブジェクトW2を作ったとき
;新しい環境E2を作り、その中のフレームで、balanceに100が束縛される。
;W2の呼び出しでは、環境E2を参照しており、W1には影響しない。

;2つの手続きオブジェクトが同じコードを共有するか、それぞれがコードの複製を持つかは、実装の細部のこと。
;4章で実装する解釈系ではコードは共有している

;図3.10
;make-withdraw手続きで、局所変数balanceをletを使って明示的に作った例
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))

;(let ((<var> <exp>)) <body>)
;((lambda (var) (body)) <exp>) のシンタックスシュガー

(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))
;示す環境構造の図をかけ

;3.2.4 内部定義
;問題3.11
;(define acc (make-account 50))
;環境E1の中に、引数balanceに50が束縛される
;make-accountが評価されて、E1の中に、手続きwithdraw、deposit、dispatchが定義される。
;変数accに、手続きdispatchが束縛される

;((acc 'deposit) 40)
;; 環境E2が作られる。dispathの引数mに、depositが束縛される
;; depositが評価されて、手続きdepositが呼び出される。
;; 環境E3が作られて、amountに40が束縛される
;; 評価されて、balanceの値が書き換わる

;;((acc 'withdraw) 60)
;;

;;(define acc2 (make-account 100))
;;make-accountが指す手続きが共通
;;balanceの値や、その下の環境などは別

;;3.3 可変データでのモデル化
;;2章のデータ抽象
;;データ構造はデータオブジェクトを作り出す構成子と、合成データオブジェクトの部分にアクセスする選択子を使って規定する。
;データオブジェクトを修正する変更し(mutator)を含んだデータ抽象を設計する

;;銀行システムのモデル化では、口座の残高を変更する必要がある。
;;銀行口座を表現するデータ構造
;; (set-balance! <account> <new-value>)
;;指定された口座の残高を、支持された新しい金額に変更する
;;変更子が定義されているデータオブジェクトを可変データオブジェクト（mutable data object）という。

;;3.3.1 可変リスト構造
;; set-car! set-cdr!
;;破壊的操作なのでビックリマークつける

(define x '((a b) c d))
(define y '(e f))
x
y
(set-car! x y)
;; xのcarをyで置き換える
;; set-cdr!はset-car!とだいたい同じ。cdrを入れ替える

;; どこからも参照されないゴミができる。ゴミ集めをする必要がありますね。
;; ただし、後々実装する処理系には組み込まない。

;;問題3.12
(define (append! x y)
  (set-cdr! (last-pair x) y)
x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)
(define w (append! x y))
w
(cdr x)

;;問題3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
x)

(define z (make-cycle (list 'a 'b 'c)))
z
;(last-pair z)

;;問題3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
(loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))
v
w
;;リストの順番をひっくりかえす

;;共有と同一
;; 3.1.3では、「同じ」と「変化」の理論的論点を話した。
;;この論点は、実際には異なるデータオブジェクト間で個々の対が共有されたときに現れる。

(define x (list 'a 'b))
(define z1 (cons x x))
x
z1
(define z2 (cons (list 'a 'b) (list 'a 'b)))
;;一般にリストをcons, car およびcdrだけを使って作用させると共有していても検出できない
;;リスト構造に変更を認めると、共有が重要となる

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
x)
z1
(set-to-wow! z1)
z2
(set-to-wow! z2)

(eq? (car z1) (cdr z1))
(eq? (car z2) (cdr z2))

;; 代入のおかげでポインタがぐちゃぐちゃしちゃってもうやだね
;; という話

;;問題3.15
;;z1とz2へのset-to-wow!の効果を説明する箱とポインタ図を書け
;;問題3.16
;;リスト構造中の対の個数を数える手続き
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

(count-pairs 'a)
(count-pairs (cons 1 2))
(count-pairs '((a b)c d))

(count-pairs '(a b c)) ;;3
(count-pairs '((b a) a)) ;;4
(define x '(a b c))
(count-pairs (cons x x)) ;;7

(define infinity 
  (let ((x '(a b c)))
    (set-cdr! (last-pair x) x)
    x))
(count-pairs infinity) ;;帰ってこない
(count-pairs '(((c) c) (c) c)) ;;7

;;ポインタが指し示す場所が重複している場合、count-pairも重複して呼び出される

;;3.17
(define (make-count-pairs walks)
  (define (count-pairs x)
    (cond ((not (pair? x)) 0)
	  ((memq x walks) 0)
	  (else
	   (set! walks (cons x walks))
	   (+ (count-pairs (car x))
	      (count-pairs (cdr x))
	      1))))
count-pairs)

;; memqについて
;; (memq x y) yにxが含まれていれば真、ないなら偽を返す
;; eq?で比べているので、参照先が違ってると同じでない

(define CP (make-count-pairs '()))
(define x (cons 'a (cons 'b (cons 'c '()))))
(CP x)
(display x)

(define x (cons 'd (cons 'a '())))
(set-car! x (cons 'b (cdr x)))
(CP x)
;gosh> 3
(display x)
;gosh> ((b a) a)#<undef>

(define x (cons 'a (cons 'b (cons 'c '()))))
(set-car! (cdr x) (cdr (cdr x)))
(set-car! x (cdr x))
(CP x)
;gosh> 3
(display x)
;gosh> (((c) c) (c) c)#<undef>

;;問題3.18
(define (circulate? items)
  (define walks '())
  (define (has-circulate? x)
    (if (memq x walks)
	#t
	(begin (set! walks (cons x walks))
	       #f)))
  (define (circulate?-iter i)
    (if (not (pair? i))
	#f
	(if (has-circulate? (car i))
	    #t
	    (circulate?-iter (cdr i)))))
(circulate?-iter items))

(define z (make-cycle (list 'a 'b 'c)))
(circulate? (list 'a 'b 'c))
(circulate? z)
(circulate? '(a b c)) ;;3
(circulate? '((b a) a)) ;;4
(define x '(a b c))
(circulate? (cons x x)) ;;7

(define infinity 
  (let ((x '(a b c)))
    (set-cdr! (last-pair x) x)
    x))
(circulate? infinity) ;;帰ってこない
(circulate? '(((c) c) (c) c)) ;;7

;;問題3.19
(define (cycle? items)
  (define (terminate? x)
    (or (null? x)
	(null? (cdr x))))
  (define (contains-loop? trace1 trace2)
    (cond ((eq? trace1 trace2) #t)
	  ((terminate? trace2) #f)
	  (else
	   (contains-loop? (cdr trace1) (cddr trace2)))))
  (if (terminate? items)
      #f
      (contains-loop? (cdr items) (cddr items))))

(cycle? '(a b c)) ;;#f
(define z (make-cycle (list 'a 'b 'c)))
(cycle? z)

;; trace1は1個づつ進む。trace2は2個づつ進む。
;; もしループしていたら、いつのまにかtrace2がtrace1の後ろにいっていつか追いつく。
;; 追いつくことがあったらそれはループしている
;; MSの入社試験？か何かにでてるらしい有名な問題

