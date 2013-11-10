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

