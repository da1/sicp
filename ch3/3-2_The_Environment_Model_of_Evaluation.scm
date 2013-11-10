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

