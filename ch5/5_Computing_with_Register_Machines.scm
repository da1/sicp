;; 5 レジスタ計算機での計算

;; 今まではLispで書いた手続きを使ってプロセスを学び、プロセスを記述した。
;; 1章 置き換えモデル
;; 3章 環境モデル
;; 4章 超循環評価器

;; 超循環評価器ですら答えていない問題
;; 例えば、部分式の評価がその値を使う式に値をどう返すか。
;; ある再帰的手続きは、再帰的プロセスを生成するのに、別の再帰的手続きは反復プロセスを生成するのはなぜか
;; Lisp評価器の制御構造を更に完全に記述するためには、Lispより更に基本的レベルで勉強しなければならない。

;; 伝統的な計算機でのステップごとの操作を行い、プロセスを記述する。
;; レジスタ計算機
;; 機械語プログラムによく似た、レジスタを操作する命令を逐次実行する
;; いくつかのLisp手続きについて、それらの手続きを実行する特別なレジスタ計算機を設計する

;; 例えば
;; 加算：2つのレジスタから取り出した数を足し、結果を第三のレジスタに格納する。簡単
;; リスト演算：car, cdrなど高度な記憶割り当て機構が必要でちょっと複雑
;; →後の章で

;; 5.1 レジスタ計算機の設計
;; レジスタ計算機の設計には、まずデータパスと、制御器を設計しなければならない。

;;
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;; このアルゴリズムを実行する計算機は、aとbの2つを覚えておく必要がある。
;; aレジスタとbレジスタに入っていると仮定

;; やること
;; (= b 0)
;; (remainder a b)

;; gcdの各サイクルで、aをbに置き換えて、bを(remainder a b)で置き換える
;; 我々のレジスタ計算機のモデルでは、各ステップで一つのレジスタにだけ新しい値を代入できる。
;; そのため、取り換えを実現するにはテンポラリのtレジスタが必要である

;; 5.1のデータバス図
;; 3ステートバッファ
;; データパス図は、計算機に必要なレジスタと演算、その接続を示す。

;; 5.2の制御図
;; ボタンを正しい順で押すための手順

;; 問題5.1
;; 階乗を計算するレジスタ計算機を設計せよ。
;; この計算機のデータパス図、制御図を描け
; t不要

;; 5.1.1 レジスタ計算機の記述言語
;; データパス図や制御図のようなものを文書の形で表現する言語を作る

;; レジスタと演算を定義して、計算機のデータパスを定義する。
;; レジスタの記述には、名前を与えてそれへの代入を制御するボタンを指定する。
;; ボタンにも名前を与えて、ボタンの制御でレジスタに入るデータを指定する。
;; 演算の記述は、演算の名前と入力を指定する。

;; 制御器は、列への入り口（entry points）を識別するラベルと共に、命令の列として定義する。
;; 命令は、レジスタへ値を代入するデータパスのボタン名
;; 指定したテストを実行するtest命令、テストに基づいて分岐させるbranch命令、goto命令など

;; 図5.3 GCD計算機の例
(data-paths
 (registers
  ((name a)
   (buttons ((name a<-b) (source (register b)))))
  ((name b)
   (buttons ((name b<-t) (source (register t)))))
  ((name t)
   (buttons ((name t<-r) (source (operation rem)))))))

(operations
 ((name rem)
  (inputs (register a) (register b)))
 ((name =)
  (inputs (register b) (constant 0))))

(controller
 test-b
 (test =)
 (branch (label gcd-done))
 (t<-r)
 (a<-b)
 (b<-t)
 (goto (label test-b))
 gcd-done)

;; 読みづらいので、データパスと制御器の情報を組み合わせて、すべてが一度に見えるようにする。
;; before:
;; 制御器で、ボタンt<-rを押せ、データパスでt<-rはrem演算の値をtに代入する
;; after:
;; aとbの内容をremして結果をtに代入するボタンを押せ
;; データパスの記述をやめて、制御器の列だけになる。

(load "../assemble.scm")
(load "../compdata.scm")
(load "../compiler.scm")
(load "../syntax.scm")
(load "../regsim.scm")

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

(set-register-contents! gcd-machine 'a 16)
(set-register-contents! gcd-machine 'b 13)
(start gcd-machine)
(get-register-contents gcd-machine 'a)

;; 図5.3のものより読みやすいが欠点もある
;; 冗長 データパスの要素が制御器の命令列に現れるときに、記述が繰り返される
;; どう見てもLisp式

;; 欠点はあるけど、データパスの要素や接続よりも制御器の理解の方に感心があるため、
;; このレジスタ計算機の言語を使う。
;; 実際の計算機の制御では、データパスの設計が重要であることを心に留めよう

;; 問題5.2
;; レジスタ計算機言語を使い、問題5.1の階乗計算機を記述せよ
(define factorial-machine
  (make-machine
   '(p c n t)
   (list (list '* *) (list '+ +) (list '> >))
'(
 (assign p (const 1))
 (assign c (const 1))
iter
 (test (op >) (reg c) (reg n))
 (branch (label factorial-done))
 (assign t (op *) (reg c) (reg p))
 (assign p (reg t))
 (assign t (op +) (reg c) (const 1))
 (assign c (reg t))
 (goto (label iter))
factorial-done)
))

(set-register-contents! factorial-machine 'n 5)
(start factorial-machine)
(get-register-contents factorial-machine 'p)

;; * 働き
;; GCD計算機の結果を端末に印字させるようにする
;; 読み込みや印字ができる計算機は、基本演算で利用可能だと仮定

;; read レジスタにどこからともなく入力された値を格納する
;; print レジスタに出力しない、効果を持つがこの効果は設計している計算機の一部ではない
;; 働き（action）と呼ぶ。
;; データパス図では、働きを演算と同じように表す
;; 制御器に働きのボタンを押させるためのperform命令を使う

;; 図5.4
(controller
gcd-loop
    (assign a (op read))
    (assign b (op read))
test
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
    (assign t (op rem) (reg a) (reg b))
    (assign a (reg b))
    (assign b (reg t))
    (goto (label test-b))
gcd-done
    (perform (op print) (reg a))
    (goto (label gcd-loop)))

;; 5.1.2 計算機設計における抽象
;; 実際な非常に複雑な基本的演算を含むように計算機を定義することがよくある。
;; 5.4節や5.5節ではSchemeの環境操作を基本的として扱う。
;; 複雑な基本的演算をいつでももっと簡単な基本演算で取り替えることができる

;; GCD計算機の剰余演算を置き換える
(define (remainder n d)
  (if (< n d)
      n
      (remainder (- n d) d)))

;; 図5.5は改善した計算機データパスと制御器

;; 図5.6
(define gcd-machine
  (make-machine
   '(a b t)
   (list (list '= =) (list '< <) (list '- -))
'(
test-b
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
    (assign t (reg a))
rem-loop
    (test (op <) (reg t) (reg b))
    (branch (label rem-done))
    (assign t (op -) (reg t) (reg b))
    (goto (label rem-loop))
rem-done
    (assign a (reg b))
    (assign b (reg t))
    (goto (label test-b))
gcd-done)
))

(set-register-contents! gcd-machine 'a 25)
(set-register-contents! gcd-machine 'b 5)
(start gcd-machine)
(get-register-contents gcd-machine 'a)

;; 問題5.3
;; 1.1.7で述べたNewton法を使い、平方根を計算する計算機を設計せよ

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

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

(controller
  (assign g (const 1.0))
iter
  (test (op good-enough?) (reg g))
  (assign t (op improve) (reg g))
  (assign g (reg t))
  (branch iter)
          )

(define newton-machine
  (make-machine
   '(g t1 t2 x)
   (list (list '* *) (list '/ /) (list '+ +) (list '- -) (list '< <) (list 'abs abs))
'(
  (assign g (const 1.0))
iter-loop
  (assign t1 (op *) (reg g) (reg g))
  (assign t2 (op -) (reg t1) (reg x))
  (assign t1 (op abs) (reg t2))
  (test (op <) (reg t1) (const 0.001))
  (branch (label iter-done))
  (assign t1 (op /) (reg x) (reg g))
  (assign t2 (op +) (reg g) (reg t1))
  (assign g (op /) (reg t2) (const 2.0))
  (goto (label iter-loop))
iter-done)
))

(set-register-contents! newton-machine 'x 9)
(start newton-machine)
(get-register-contents newton-machine 'g)

;; 5.1.3 サブルーチン
;; 図5.7 2つのGCD計算を持つ計算機のデータパスと制御器命令列の一部
;; ベタ書き

;; 図5.8 2つの異なるGCD計算に同じデータパスを使う計算機の制御器命令列の一部
;; 使うレジスタを共通化する
gcd-1
    (test (op =) (reg b) (const 0))
    (branch (label after-gcd-1))
    (assign t (op rem) (reg a) (reg b))
    (assign a (reg b))
    (assign b (reg t))
    (goto (label gcd-1))
after-gcd-1
...
gcd-2
    (test (op =) (reg b) (const 0))
    (branch (label after-gcd-2))
    (assign t (op rem) (reg a) (reg b))
    (assign a (reg b))
    (assign b (reg t))
    (goto (label gcd-2))
after-gcd-2

;; 図5.9 図5.8の重複命令列を避けるためにcontinueレジスタを使う
;; ラベルを共通化
gcd
  (test (op =) (reg b) (const 0))
  (branch (label gcd-done))
  (assign t (op rem) (reg a) (reg b))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label gcd))
gcd-done
  (test (op =) (reg continue) (const 0))
  (branch (label after-gcd-1))
  (goto (label after-gcd-2))
...
  (assign continue (const 0))
  (goto (label gcd))
after-gcd-1
...
  (assign continue (const 1))
  (goto (label gcd))
after-gcd-2

;; 重複したデータパスの部品を戻した。
;; 制御器は入り口が違うだけの2つのGCDの列を持つ。
;; この2つの列を、その最後で重要な命令列の正しい場所へ分岐し戻る一つの列（gcd）サブルーチン
;; で置き換えられればなお良い。

;; gcdへ分岐する前に、識別用の値を特別なレジスタに置く
;; gcdサブルーチンの終わりで、continueレジスタの値に従って、after-gcd-1かafter-gcd-2に戻る

;; 図5.10 continueレジスタにラベルを代入すると、図5.9に示した戦略を単純化、一般化できる
gcd
  (test (op =) (reg b) (const 0))
  (branch (label gcd-done))
  (assign t (op rem) (reg a) (reg b))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label gcd))
gcd-done
  (goto (reg continue))
...
  (assign continue (label after-gcd-1))
  (goto (label gcd))
after-gcd-1
...
  (assign continue (label after-gcd-2))
  (goto (label gcd))
after-gcd-2

;; continueレジスタにラベルを格納できるように、assign命令を拡張する

;; サブルーチンが複数ある場合は、複数の継続レジスタを使うか、単一のレジスタをすべてのサブルーチンが共有するか
;; 共有は経済的だが、サブルーチンが更に別のサブルーチンを呼ぶ場合などに注意が必要である。

;; 5.1.4 再起を実装するためのスタックの使用
;; これまでに示した考え方で、プロセスの各状態変数に対応するレジスタを持つレジスタ計算機を規定すれば、
;; 反復的プロセスを実装できる

;; 再帰的プロセスの実装は追加の機構を必要とする
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;; 再起プロセスの場合、新しい階乗下請け問題の答えは元々の問題の答えではない
;; 下請け問題の下請け問題の下請けの...の問題が必要になる。
;; 同じ部品を計算機の入れ子の各実態として使うようにすれば、階乗プロセスをレジスタ計算機として実装できる

;; 下請け問題では、レジスタの内容は主問題のものとは違う。
;; 中断した計算を続行するときに、元の状態に回復できるっように、必要な値を退避させないといけない。

;; レジスタの値を退避するのにスタックを使う。
;; 2つの命令を使いする
;; save 値をスタックに置く
;; restore スタックから回復する

;; スタックによって、階乗計算のデータパスの単一のコピーを、階乗の下請けの各問題に再利用できる
;; 制御器についても同じ
;; 命令の下請け問題を解く部分へ移行し、主問題から呼び出したところから続行するために制御器にcontinueレジスタを使わせる

;; 図5.11 再帰的階乗計算機
(define fact-machine
  (make-machine
   '(n val continue)
   (list (list '= =) (list '- -) (list '* *))
'(
    (assign continue (label fact-done))
 fact-loop
    (test (op =) (reg n) (const 1))
    (branch (label base-case))
    ;; 再起呼び出しの設定
    (save continue)
    (save n)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-fact))
    (goto (label fact-loop))
after-fact
    (restore n)
    (restore continue)
    (assign val (op *) (reg n) (reg val))
    (goto (reg continue))
base-case
    (assign val (const 1))
    (goto (reg continue))
fact-done)
))

(set-register-contents! fact-machine 'n 5)
(start fact-machine)
(get-register-contents fact-machine 'val)

;; * 二重再帰
;; 1.2.2のFibonacci数の木構造再帰計算を考える
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;; 階乗と同様に、再帰的Fibonacci計算をレジスタn, valおよびcontinueを持つレジスタ計算機として実装できる
;; この計算機は、再帰呼び出しを実行しなければならない場所が、命令列に2つあるので、階乗の時より複雑になる

;; 図5.11 再帰的階乗計算機
(define fib-machine
  (make-machine
   '(n val continue)
   (list (list '< <) (list '+ +) (list '- -))
'(
    (assign continue (label fib-done))
fib-loop
    (test (op <) (reg n) (const 2))
    (branch (label immediate-answer))
    ;;Fib(n-1)
    (save continue)
    (assign continue (label afterfib-n-1))
    (save n)
    (assign n (op -) (reg n) (const 1))
    (goto (label fib-loop))
afterfib-n-1
    (restore n)
    (restore continue)
    ;;Fib(n-2)
    (assign n (op -) (reg n) (const 2))
    (save continue)
    (assign continue (label afterfib-n-2))
    (save val)
    (goto (label fib-loop))
afterfib-n-2
    (assign n (reg val))
    (restore val)
    (restore continue)
    (assign val (op +) (reg val) (reg n))
    (goto (reg continue))
immediate-answer
    (assign val (reg n))
    (goto (reg continue))
fib-done)
))

(set-register-contents! fib-machine 'n 6)
(start fib-machine)
(get-register-contents fib-machine 'val)

;; 必要になるレジスタを退避して、nレジスタをそのFibを再帰的に計算したい値（n-1かn-2）に設定して、
;; continueに主命令列のそこへ戻りたい入り口を代入する。次にfib-loopに行く

;; 問題5.4
;; 次の手続きのそれぞれを実装するレジスタ計算機を規定せよ
;; 各計算機に対し、制御器の命令列を書き、データパスを示す図を描け
;; a. 再帰的べき乗
(define expt-machine
  (make-machine
   '(n b val continue)
   (list (list '= =) (list '- -) (list '* *))
'(
  (assign continue (label expt-done))
expt-loop
  (test (op =) (reg n) (const 0))
  (branch (label base-case))
  (save continue)
  (save n)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label after-expt))
  (goto (label expt-loop))
after-expt
  (restore n)
  (restore continue)
  (assign val (op *) (reg b) (reg val))
  (goto (reg continue))
base-case
  (assign val (const 1))
  (goto (reg continue))
expt-done)))

(set-register-contents! expt-machine 'b 2)
(set-register-contents! expt-machine 'n 4)
(start expt-machine)
(get-register-contents expt-machine 'val)

(define expt-machine
  (make-machine
   '(p c b n)
   (list (list '- -) (list '* *) (list '= =))
'(
  (assign p (const 1))
  (assign c (reg n))
expt-iter
  (test (op =) (reg c) (const 0))
  (branch (label expt-done))
  (assign c (op -) (reg c) (const 1))
  (assign p (op *) (reg b) (reg p))
  (goto (label expt-iter))
expt-done)))

(set-register-contents! expt-machine 'b 2)
(set-register-contents! expt-machine 'n 4)
(start expt-machine)
(get-register-contents expt-machine 'p)

;; 問題5.5 階乗とFibonacci計算機を机上シミュレーションせよ

;; 問題5.6
;; Ben BitdiddleはFibonacci計算機の制御列には、余計なsaveとrestoreがあると見た。それはどれか
(define fib-machine
  (make-machine
   '(n val continue)
   (list (list '< <) (list '+ +) (list '- -))
'(
    (assign continue (label fib-done))
fib-loop
    (test (op <) (reg n) (const 2))
    (branch (label immediate-answer))
    ;;Fib(n-1)
    (save continue)
    (assign continue (label afterfib-n-1))
    (save n)
    (assign n (op -) (reg n) (const 1))
    (goto (label fib-loop))
afterfib-n-1
    (restore n)
;    (restore continue) ;無駄
    ;;Fib(n-2)
    (assign n (op -) (reg n) (const 2))
;    (save continue)    ;無駄 上でrestoreしたものをすぐにsaveしてる
    (assign continue (label afterfib-n-2))
    (save val)
    (goto (label fib-loop))
afterfib-n-2
    (assign n (reg val))
    (restore val)
    (restore continue)
    (assign val (op +) (reg val) (reg n))
    (goto (reg continue))
immediate-answer
    (assign val (reg n))
    (goto (reg continue))
fib-done)
))

(set-register-contents! fib-machine 'n 6)
(start fib-machine)
(get-register-contents fib-machine 'val)


;; 5.1.5 命令の要約
;; レジスタ計算機言語の制御器命令一覧
(assign <register-name> (reg <register-name>))
(assign <register-name> (const <constant-value>))
(assign <register-name> (op <operation-name>) <input1> .. <inputn>)
(perform (op <operation-name>) <input1> .. <inputn>)
(test (op <operation-name>) <input1> .. <inputn>)
(branch (label <label-name>))
(goto (label <label-name>))
(assign <register-name> (label <label-name>))
(goto (reg <register-name>))
(save <register-name>)
(restore <register-name>)
