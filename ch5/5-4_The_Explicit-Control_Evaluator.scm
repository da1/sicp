;; 5.4 積極制御評価器
;; 5.1では、単純なSchemeプログラムを、レジスタ計算機の記述へどう変換するかを見た。
;; ここでは更に複雑なプログラム、つまりSceme解釈系の振る舞いが手続きevalとapplyを使ってどう記述できるかを示す
;; 本節で開発する積極制御評価器（explicit-control evaluator）は、評価処理系での手続呼び出しと引数渡しの機構が、レジスタとスタックでどう記述できるか示す

;; * レジスタと演算
;; 我々のScheme評価器のレジスタ計算機には、スタックと7つのレジスタ
;; exp, env, val, continue, proc, argl, unev
;; がある

;; 5.4.1 積極制御評価器の中核
;; 評価器の中心部分は、eval-dispatchから始まる命令の列である 
;; 4.1.1で述べた超循環評価器のeval手続に対応している

eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

;; * 単純式の評価
;; 整数、文字列、変数、クォート式、lambda式は、実行すべき部分式がないため、
;; 評価器は、valに値をセットしてcontinueで指定した入り口から実行を続行するだけでよい

ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))
ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))
ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure)
	  (reg unev) (reg exp) (reg env))
  (goto (reg continue))

;; * 手続作用の評価
;; 手続作用とは、演算子と非演算子の組み合わせである。
;; 演算子は、結果が手続となる部分式で、非演算子は、その値が引数になる部分式である
;; 超循環evalは、組み合わせの各要素を評価すべく、自分を再帰的に呼び出して作用を扱い、
;; 結果を実際に手続作用を実行するapplyに渡す

ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))

;; 評価から戻ったら、組み合わせ被演算子の評価と、arglに保持したリストへの結果の引数の蓄積に進む
ev-appl-did-operator
  (restore unev) ;; 被演算子
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)

;; 引数評価ループの各サイクルでは、unevリストの被演算子を評価して結果をarglに蓄積する
ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))

ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))

ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))

;; 
(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))

;; * 手続き作用
;; 超循環評価器のapplyに相当する、apply-dispatch
;; apply-dispachに来た時点で、procには作用させる手続きが、arglには手続きを作用させるべき評価済みの引数のリストがある
apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))
  (branch (label compound-apply))
  (goto (label unknown-prcedure-type))

;; 基本手続きのそれぞれは、引数をarglからとり、結果をvalへ置く
;; proc手続きをarglの引数に作用させるapply-primitive-procedure演算を使う
primitive-apply
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
  (goto (reg continue))

;; 合成手続きは、超循環評価器と同じように行う
;; 手続きのパラメータを引数に束縛するフレームを作り、手続きが持ち込んだ環境をこのフレームで拡張する
;; この拡張された環境で、手続きの本体となる式の並びを評価する
;; compound-applyは、解釈系でenvレジスタに新しい値を代入する唯一の場所である。
;; 超循環評価器と同様に、手続きの持ち込んだ環境と、引数リストと束縛される変数の対応するリストから、新しい環境を構成する

compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment)
	  (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))

;; 5.4.2 並びの評価と末尾評価器
;; begin式は、評価すべき式の並びをunevに置き、continueをスタックに退避させる
ev-begin
  (assign unev)
  (save continue)
  (goto (label ev-sequence))

;; 手続き本体にある暗黙の並びを扱うには、compound-applyからev-sequenceへ行く
;; continueはev-applicationでスタックに退避されている

;; ev-sequenceとev-sequence-continueは、並びの式を順に評価するループを形成する
;; 未評価の式のリストは、unevに保存する

ev-sequence
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence
  (restore continue)
  (goto (label eval-dispatch))

;; * 末尾再帰
;; 手続きが、自分自身を呼び出し続けても、記憶場所を増やす必要なしに手続きが実行できる評価器を末尾再帰的評価器という
;; 我々の評価器は、並びの最後の式を評価するのに、情報をスタックに退避せず直接eval-dispatchへ行くので末尾再帰的である.

;; 5.4.3 条件式、代入および定義
;; if式では、帰結部が代替え部をあとから取り出せるように、条件式の評価時にはif自身を退避させる
ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label eval-dispatch))

ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))
ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))

;; * 代入と定義
ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch))
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op set-variable-value!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))

ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch))
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))

;; 問題5.23
;; 評価器を拡張し、cond, letなどの導出された式を扱えるようにせよ

;; 問題5.24
;; condをifには簡約せず、新しい基本的特殊形式として実装せよ
;; cond節の述語を順に、真のものを見つけるまでテストするループを構成し、次にev-sequenceを使い、その節の行動を評価しなければならない

;; 問題5.25
;; 評価器を修正し、4.2節の遅延評価器に基づいた正規順序の評価が使えるようにせよ

;; 5.4.4 評価の実行
;; 評価器計算機に駆動ループを組み込む
read-eval-print-loop
  (perform (op initialize-stack))
  (perform
   (op prompt-for-input) (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label eval-dispatch))
print-result
  (perform
   (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

;; エラーを見つけたら、エラーメッセージを印字して駆動ループに戻る
unknown-expression-type
  (assign val (const unknown-expression-type-error))
  (goto (label signal-error))

unknown-procedure-type
  (restore continue)
  (assign val (const unknown-prcedure-type-error))
  (goto (label signal-error))

signal-error
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

;;
(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   '(
     read-eval-print-loop
     )))

(define eceval-operations
  (list (list 'self-evaluating? self-evaluating?)
	))

(define the-global-environment (setup-environment))

(start eceval)