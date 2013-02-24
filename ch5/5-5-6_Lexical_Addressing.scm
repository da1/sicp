;; 5.5.6 文面アドレス
;; 翻訳系による最適化の最も普通のものの一つは、変数探索の最適化である

;; 今までの翻訳系
;; 評価計算機のlookup-variable-value を使う
;; 実行時のフレームから、順々に外側に向かって変数を探す
;; これは、フレームが深く入れ子になっていたり、変数が多い場合に効率が悪い

(let ((x 3) (y 3))
  (lambda (a b c d e)
    (let ((y (* a b x))
	  (z (+ c d x)))
      (* x y z))))

((lambda (x y)
   (lambda (a b c d e)
     ((lambda (y z) (* x y z))
      (* a b x)
      (+ c d x))))
 3
 4)

;; lookup-variable-valueがxを探すと、
;; 2フレーム外の、最初の変数で見つかる

;; これを、新しい変数探索演算lexical-address-lookupを使って調べる。
;; 引数に、環境と、読み飛ばすフレーム数と、フレーム内で何個の変数を読み飛ばす変異数からなる文面アドレスを取る。
;; 同様に、翻訳したコードあ、set-variable-value!の代わりに、lexical-address-set!を使うことができる

((lambda (x y)
   (lambda (a b c d e)
     ((lambda (y z) <e1>)
      <e2>
      (+ c d x))))
 3
 4)
;; 文面アドレスは、参照する場所によって変わる
;; xのアドレスは、e1では(2,0), e2では(1,0)

;; 翻訳系に文面アドレスを使ったコードを作り出させる方法は、翻訳時環境というデータ構造を維持することである。
;; 変数アクセス演算をするときに、どの変数が、実行時環境において
;; どのフレームのどの場所にあるかを覚えておく

;; compileの引数に追加して、コード生成器に渡される。


;; 問題5.39
;; lexical-address-lookupを書け
;; 文面アドレスと実行時環境を受け取り、指定した文面アドレスに格納した変数の値を返す
;; また、指定した文面アドレスにある変数の値を変更する演算を実装する手続きlexical-address-set!を書け

;; http://www.serendip.ws/archives/3762
(define (lexical-address-frame-number lexical-address)
  (car lexical-address))

(define (lexical-address-displacement-number lexical-address)
  (cadr lexical-address))

(define (make-lexical-address frame-number displacement-number)
  (list frame-number displacement-number))

(define (lexical-address-lookup lexical-address env)
  (let ((frame-num (lexical-address-frame-number lexical-address))
	(displacement-num (lexical-address-displacement-number lexical-address)))
    (let ((frame (list-ref env frame-num)))
      (let ((value (list-ref (frame-values frame) displacement-num)))
	(if (eq? value '*unassigned*)
	    (error "Unassined variable")
	    value)))))

(define (lexical-address-set! lexical-address val env)
  (let ((frame-num (lexical-address-frame-number lexical-address))
	(displacement-num (lexical-address-displacement-number lexical-address)))
    (let ((frame (list-ref env frame-num)))
      (define (iter vrs vls count)
	(cond ((null? vrs)
	       (error "Unbound variable - LEXICAL-ADDRESS-SET!"))
	      ((= count 0)
	       (set-car! vls val))
	      (else
	       (iter (cdr vrs) (cdr vls) (- count 1)))))
      (iter (frame-variables frame) (frame-values frame) displacement-num))))


;; 問題5.40
;; 翻訳系を修正し、上述の翻訳時環境を維持するようにせよ。
;; compileとそれぞれのコード生成器に翻訳時環境の引数を加え、compile-lambda-bodyで翻訳時環境を拡張せよ

;; 問題5.41
;; 引数として変数と翻訳時環境をとり、その環境に対する変数の文面アドレスを返す手続きfind-variableを書け
(define (find-variable var ct-env)
  (define (frame-iter var frames frame-number)
    (if (null? frames)
	'not-found
	(let ((addr (scan-iter var (car frames) frame-number 0)))
	  (if (null? addr)
	      (frame-iter var (cdr frames) (+ frame-number 1))
	      addr))))
  (define (scan-iter var frame frame-number displacement-number)
    (if (null? frame)
	'()
	(if (eq? var (car frame))
	    (make-lexical-address frame-number displacement-number)
	    (scan-iter var (cdr frame) frame-number (+ displacement-number 1)))))
  (frame-iter var ct-env 0))

(find-variable 'c '((y z) (a b c d e) (x y)))
;; (1 2)

(find-variable 'x '((y z) (a b c d e) (x y)))
;; (2 0)

(find-variable 'w '((y z) (a b c d e) (x y)))
;; not-found

;; 問題5.42
;; find-variableを使い、compile-variableとcompile-assignmentを書きなおして、文面アドレス命令を出力するようにせよ

(define false #f)
(define true #t)
(load "./load-eceval-compiler")
(load "./syntax")
(load "./ch5-ct-compiler")

(define (parse-compiled-code lis)
  (if (not (null? lis))
      (begin
	(if (pair? (caar lis))
	    (map (lambda (x)
		   (if (symbol? x)
		       (print x)
		       (print "  " x)))
		 (car lis))
	    (print (car lis)))
	(parse-compiled-code (cdr lis)))))

(parse-compiled-code
 (compile
  '(lambda (x y)
     (lambda (a b)
       (+
	(+ x a)
        (* y b)
        (set! x a)
        (set! z b))))
  'val 'next '()))

;; 問題5.43
;; 4.1.6でブロック構造の内部定義は真のdefineと考えるべきでないと論じた
;; むしろ、手続き本体は定義している内部変数は、set!を使って正しい値に初期化する通常のlambda変数として組み込まれているように解釈するべきである
;; 翻訳系を修正し、手続き本体を翻訳する前に、4.1.6と問題4.16のように内部定義を吐き出してこれを実現するようにせよ

(lambda <vars>
  (define u <e1>)
  (define v <e2>)
  <e3>)

(lambda <vars>
  (let ((u '*unassigned*)
	(v '*unassigned*))
    (set! u <e1>)
    (set! v <e2>)
    <e3>))

;; 4.16で作ったscan-out-definesを持ってきた
(define (scan-out-defines body)
  (define (iter exp vars sets exps)
    (if (null? exp)
	(list (reverse vars) (reverse sets) (reverse exps))
	(if (definition? (car exp))
	    (iter (cdr exp)
		  (cons (list (definition-variable (car exp)) ''*unassigned*) vars)
		  (cons (list 'set! (definition-variable (car exp))
			      (definition-value (car exp))) sets)
		  exps)
	    (iter (cdr exp) vars sets (cons (car exp) exps)))))
  (define (include-define? exp)
    (if (null? exp)
	#f
	(if (definition? (car exp))
	    #t
	    (include-define? (cdr exp)))))
  (if (include-define? body)
      (let ((var-val-exp-list (iter body '() '() '())))
	(list (cons 'let (cons (car var-val-exp-list)
			       (append (cadr var-val-exp-list)
				       (caddr var-val-exp-list))))))
      body))

(define (compile-lambda-body exp proc-entry ct-env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl)
      '(env)
      `(,proc-entry
	(assign env (op compiled-procedure-env) (reg proc))
	(assign env
		(op extend-environment)
		(const ,formals)
		(reg argl)
		(reg env))))
     (compile-sequence
      (scan-out-defines (lambda-body exp))
      'val 'return (cons formals ct-env)))))

(parse-compiled-code
 (compile
  '(define foo (lambda (x)
		 (define a 1)
                 (define b 2)
		 (* (+ a x) b)))
  'val 'return '()))


;; 5.5.7 翻訳したコードと評価器のインタフェース

;; scheme式を翻訳し、結果の目的コードを評価器計算機にロードし、計算機に評価器の大域環境でコードを走らせ、結果を印字し、評価器の駆動ループに入る手続きcompile-and-goを実装する

;; 評価器を修正し、翻訳される式が、解釈されるものだけでなく翻訳したコードも呼び出せるようにしよう

;; 翻訳器が翻訳したコードを扱えるように、apply-dispatch(5.4.1)を修正する
;; 翻訳した手続きを認識するようにして、制御を翻訳したコードの入り口に直接飛ぶようにする
apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))
  (branch (label compound-apply))
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-apply))
  (goto (label unknown-procedure-type))

compiled-apply
  (restore continue)
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

;; apply-dispatchで継続がスタックの一番上にあった
;; compiled-applyで継続をcontinueに戻す必要があることに注意

  (branch (label external-entry)) ; flagが設定してあれば分岐する
read-eval-print-loop
  (perform (op initialize-stack))

external-entry
  (perform (op initialize-stack))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (reg val))

(define (compile-and-go expression)
  (let ((instructions
	 (assemble (statements
		    (compile expression 'val 'return))
		   eceval)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))

(define (start-eceval)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag #f)
  (start eceval))

;; 実行する

(load "./load-eceval-compiler")
(load "./ch5-compiler")
(compile-and-go
 '(define (factorial n)
    (if (= n 1)
	1
	(* (factorial (- n 1)) n))))

(parse-compiled-code
(compile
  '(define (factorial n)
    (if (= n 1)
	1
	(* (factorial (- n 1)) n)))
  'val 'next))

;; 解釈版と、スタックの呼び出し回数、深さを比べてみる
(load "./load-eceval")
(start eceval)
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;; * 解釈と翻訳
;; 本節のプログラムを使えば、解釈と翻訳の実行戦略の切り替えができる。
;; 解釈系は、プログラム実行のステップが、抽象を使って組織化されており、対話的な開発や虫取りに優れている
;; 翻訳系は、プログラム実行のステップが、機械語で組織化されており、高速に実行できて、翻訳系ならば最適化も楽にできる

;; 解釈と翻訳の切り替えは、言語を新しい計算機へ移植する別の戦略をもたらす
;; 解釈系なら、積極制御評価器の命令を新しい計算機の命令に翻訳する
;; 翻訳系ならば、新しい計算機の言語を生成できるように、コード生成器を変更する
;; または、翻訳系自身を翻訳し、新しいn計算機の上で他のLispプログラムを翻訳させる
;; あるいは、4.1の解釈系の一つを翻訳して新しい計算機の上で走る解釈系を作ることもできる

;; 問題5.45
;; 翻訳したコードの使うスタック演算を、同じ計算に対する評価器の使うスタック演算と比べると
;; 速度とスペース両方で翻訳系がスタックの使い方を最適化する程度を決めることができる

;; a. 問題5.27で、n!の計算の評価器が必要なプッシュの回数とスタックの深さをnの関数として決めた。問題5.14では、特殊目的の階乗計算機で同じ計測をした。
;; factorialで同じ解析をせよ
N, push,depth
(define a (list
'(3 19 8)
'(4 25 11)
'(5 31 14)
'(6 37 17)
'(7 43 20)
'(8 49 23)
'(9 55 26)
'(10 61 29)
'(1000 6001 2999)
'(10000 60001 29999)
'(100000 600001 299999)
))

pushes = 6N+1
depth = 3N-1

;; 翻訳版と解釈版のプッシュ回数、スタックの深さそれぞれで比率を出せ
;; 同様に、特殊目的の計算機のスタック利用と解釈版の利用の比を求めよ
解釈版
N
(define b (list
'(3 80 18)
'(4 112 23)
'(5 144 28)
'(6 176 33)
'(7 208 38)
'(8 240 43)
'(9 272 48)
'(10 304 53)
'(1000 31984 5003)
'(10000 319984 50003)
'(100000 3199984 500003)
))

;; 32n-16
;; 5n+3

(define (rate-push a b)
  (exact->inexact (/ (cadr a) (cadr b))))
(define (rate-depth a b)
  (exact->inexact (/ (caddr a) (caddr b))))

(define (f rate a1 a2)
  (cond ((null? a1) '())
	(else
	 (let ((n1 (car a1))
	       (n2 (car a2)))
	   (begin
	     (display
	      (list (car n1) (rate n1 n2)))
	     (newline)
	     (f rate (cdr a1) (cdr a2)))))))

;; 解釈/翻訳の比
;; push 5.33
;; depth 1.66
(f rate-push b a)
(f rate-depth b a)

(load "./5-2_A_Register-Machine_Simulator")

(define c (list
'(3 4 4)
'(4 6 6)
'(5 8 8)
'(6 10 10)
'(7 12 12)
'(8 14 14)
'(9 16 16)
'(10 18 18)
'(1000 1998 1998)
'(10000 19998 19998)
'(100000 199998 199998)
))

;; 特殊目的/翻訳の比
;; push 0.33
;; depth 0.66
(f rate-push c a)
(f rate-depth c a)


;; 特殊目的のコードと解釈されるコードの比を、翻訳するコードと解釈されるコードの比と比較せよ
;; 特殊目的コード 28行
;; 翻訳コード 81行
;; 3倍くらい 環境の退避回復、手続きの探索の分

;; b. 性能において、手で加工した版に近づくようなコードを生成するような翻訳系の改良は何か
;; 環境、

  ;; (assign proc (op lookup-variable-value) (const -) (reg env))
  ;; (assign val (const 1))
  ;; (assign argl (op list) (reg val))
  ;; (assign val (op lookup-variable-value) (const n) (reg env))
  ;; (assign argl (op cons) (reg val) (reg argl))
  ;; (test (op primitive-procedure?) (reg proc))


;; 問題5.46
;; 問題5.45のような解析をして、図5.12の特殊目的Fibonacci計算機と比較して、木構造再帰のFibonacci手続きの翻訳の有効性を決めよ

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(load "./load-eceval-compiler")
(load "./ch5-compiler")
(compile-and-go
'(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2))))))

(define a (list
'(3 27 8)
'(4 47 11)
'(5 77 14)
'(6 127 17)
'(7 207 20)
'(8 337 23)
'(9 547 26)
'(10 887 29)
'(11 1437 32)
'(12 2327 35)
'(13 3767 38)
'(20 109457 59)
'(10000 1 29999)
))

;; S(n) = S(n-1) + S(n-2) + 3
;; 3n-1

;; (parse-compiled-code
;; (compile
;; '(define (factorial n)
;;   (if (= n 1)
;;       1
;;       (* (factorial (- n 1)) n))) 'val 'next))

(load "./load-eceval")
(start eceval)
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define b (list
'(3 128 18)
'(4 240 23)
'(5 408 28)
'(6 688 33)
'(7 1136 38)
'(8 1864 43)
'(9 3040 48)
'(10 4944 53)
'(11 8024 58)
'(12 13008 63)
'(13 21072 68)
'(20 612936 103)
'(10000 1 50003)
))
;; push  S(n) = S(n-1) + S(n-2) + 40
;; depth 5n+3

(f rate-push b a)
(f rate-depth b a)
;; push 5.99
;; depth 1.666

;; 問題5.47
;; 翻訳した手続きが、合成手続き（解釈する手続き）を呼び出せない

(compile-and-go
  '(begin
     (define (g x) (+ x 10))
     (define (f x) (g x))))

(f 1)

(define (g x) (+ x 20))

(f 1)

;; 呼び出せるように、compile-procedure-call, ecevalを修正する
(load "./load-eceval-compiler")
(load "./q547")

;; 問題5.48
;; compile-and-run

;; compile-and-runを基本手続きとして追加する
(define (setup-environment2)
  (extend-environment
    (list 'compile-and-run)
    (list (list 'primitive compile-and-run))
    (setup-environment)))

(define (compile-and-go expression)
  (let ((instructions
	 (assemble (statements
		    (compile expression 'val 'return))
		   eceval)))
    (set! the-global-environment (setup-environment2))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))

(define (compile-and-run? proc)
  (tagged-list? proc 'compile-and-run))

(define (compile-and-run expression)
  (let ((instructions
          (assemble (statements
                      (compile expression 'val 'return))
                    eceval)))
       (set-register-contents! eceval 'val instructions)
       (set-register-contents! eceval 'flag true)
       (start eceval)))

(load "./load-eceval-compiler")
(load "./q548")
(compile-and-go
  '(define (square x) (* x x)))

;;; EC-Eval input:
(square 4)

(factorial 5)

(define (factorial2 n)
  (if (= n 1)
      1
      (* (factorial2 (- n 1)) n)))

(factorial2 5)