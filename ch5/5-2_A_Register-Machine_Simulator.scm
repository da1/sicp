;; 5.2 レジスタ計算機シミュレータ

;; (make-machine <register-name>> <operations> <controller>)
;; 与えられたレジスタ、演算および制御器を持つ計算機のモデルを構築し、それを返す

;; (set-register-contents! <machine-model> <register-name> <value>)
;; 与えられた計算機のシミュレートされるレジスタに値を格納する

;; (get-register-contents <machine-model> <register-name>)
;; 与えられた計算機のレジスタの内容を返す

;; (start <machine-model>)
;; 計算機のシミュレートを実行する

(define ghd-machine
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

(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)
(start gcd-machine)
(get-register-contents gcd-machine 'a)

;; 問題5.7
;; このシミュレータを使い、問題5.4で設計した計算機をテストせよ
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

;; 5.2.1 計算機モデル
;; make-machineは、3章で開発したメッセージパッシング技法を使う
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
		((machine 'allocate-register) register-name))
	      register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; * レジスタ
;; レジスタは局所状態を持つ手続き
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
	    ((eq? message 'set)
	     (lambda (value) (set! contents value)))
	    (else
	     (error "Unknown request -- REGISTER" message))))
    dispatch))

;; レジスタへのアクセス用
(define (get-contents register)
  (register 'get))
(define (set-contents register value)
  ((register 'set) value))

;; *スタック
;; スタックも局所状態を持った手続きである
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
	  (error "Empty stack -- POP")
	  (let ((top (car s)))
	    (set! s (cdr s))
	    top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
	    ((eq? message 'pop) pop)
	    ((eq? message 'initialize) (initialize))
	    (else (error "Unknown request -- STACK"
			 message))))
    dispatch))

;; スタックへのアクセス用
(define (pop stack)
  (stack 'pop))
(define (push stack value)
  ((stack 'push) value))

;; * 基本計算機
;; make-new-machine手続きは、スタック、空の命令列、
;; 演算のリスト（スタックを初期化する命令を含む）、レジスタ表（flagとpc(program counter)を含む）
;; を局所状態とするオブジェクトを構成する
;; flagレジスタは、分岐制御に使う。test命令がflagの内容を設定し、branch命令でflagの内容を見て分岐するしないを決定する
;; pcレジスタは、実行する命令の進行を制御する。シミュレーションが進むとpcは実行すべき次の命令列を指し、executeがそれをとって実行する

;; 計算機のstart演算の手続きインタフェースを定義する
(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))


;; 5.2.2 アセンブラ
;; アセンブラは、ある計算機の制御器の式の列を、それぞれが実行手続きを持つ対応する機械命令のリストに変換する
(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence '()))
    (let ((the-ops
	   (list (list 'initialize-stack
		       (lambda () (stack initialize)))))
	  (register-table
	   (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
	(if (assoc name register-table)
	    (error "Multiply defined register:" name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
	      (error "Unknown register:" name))))
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (begin
		((instruction-execution-proc (car insts)))
		(execute)))))
      (define (dispatch message)
	(cond ((eq? message 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      ((eq? message 'install-instruction-sequence)
	       (lambda (seq) (set! the-instruction-sequence seq)))
	      ((eq? message 'allocate-register) allocate-register)
	      ((eq? message 'get-register) lookup-register)
	      ((eq? message 'install-operations)
	       (lambda (ops) (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack) stack)
	      ((eq? message 'operations) the-ops)
	      (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (assemble controller-text machine)
  (extract-labels controller-text
		  (lambda (insts labels)
		    (update-insts! insts labels machine)
		    insts)))

;; 命令のリストとラベルを対応付ける表を作る
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
		      (lambda (insts labels)
			(let ((next-inst (car text)))
			  (if (symbol? next-inst)
			      (receive insts
				       (cons (make-label-entry next-inst
							       insts)
					     labels))
			      (receive (cons (make-instruction next-inst)
					     insts)
				       labels)))))))

;;update-insts! 命令の文書を持っていただけの命令リストを、対応する手続きを含むようにする
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
	(flag (get-register machine 'flag))
	(stack (machine 'stack))
	(ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
	inst
	(make-execution-procedure
	 (instruction-text inst) labels machine
	 pc flag stack ops)))
     insts)))

;; 機械命令データ構造
;; 命令文書を対応する実行手続きとついにする
(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
	(cdr val)
	(error "Undefined label -- ASSEMBLE" label-name))))

;; 問題5.8
;; ラベルhereが複数回定義してあって曖昧である。
;; start
;;   (goto (label here))
;; here
;;   (assign a (const 3))
;;   (goto (label there))
;; here
;;   (assign a (const 4))
;;   (goto (label there))
;; there
;; シミュレータでは、thereに達したときのaの値はどうなるか
;; extract-labels手続きを修正して、同じラベル名が異なる2つの場所を指すように使われたらエラーとせよ
(load "../assemble.scm")
(load "../compdata.scm")
(load "../compiler.scm")
(load "../syntax.scm")
(load "../regsim.scm")
(define here-machine
  (make-machine
   '(a)
   (list '())
'(
start
  (goto (label here))
here
  (assign a (const 3))
  (goto (label there))
here
  (assign a (const 4))
  (goto (label there))
there
)))

(start here-machine)
(get-register-contents here-machine 'a)

;; http://sicp-study.g.hatena.ne.jp/papamitra/20080728/sicp_ex5_8
;; extract-labelsはその再帰具合からtextの末尾(ここではthere)からlabelsを作っていく。一方make-gotoで使用するlookup-labelではassocでlabelsリストの先頭から検索していくので、最初のhereに飛ぶことになる。
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
		      (lambda (insts labels)
			(let ((next-inst (car text)))
			  (if (symbol? next-inst)
			      (if (assoc next-inst labels)
				  (error "Multiply defined label: " next-inst)
                		  (receive insts
				      (cons (make-label-entry next-inst insts)
					    labels)))
			          (receive (cons (make-instruction next-inst)
					   insts)
				           labels)))))))


;; 5.2.3 命令の実行手続きの生成
;; アセンブラは命令の実行手続きを生成すべく、make-execution-procedureを呼び出す
(define (make-execution-procedure inst labels machine
				  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
	 (make-assign inst machine labels ops pc))
	((eq? (car inst) 'test)
	 (make-test inst machine labels ops pc))
	((eq? (car inst) 'branch)
	 (make-branch inst machine labels flag pc))
	((eq? (car inst) 'goto)
	 (make-goto inst machine labels pc))
	((eq? (car inst) 'save)
	 (make-save inst machine stack pc))
	((eq? (car inst) 'restore)
	 (make-restore inst machine stack pc))
	((eq? (car inst) 'perform)
	 (make-perform inst machine labels ops pc))
	(else (error "Unknown instruction type -- ASSEMBLE" inst))))

;; * assign命令
(define (make-assign inst machine labels operations pc)
  (let ((target
	 (get-register machine (assign-reg-name inst)))
	(value-exp (assign-value-exp inst)))
    (let ((value-proc
	   (if (operation-exp? value-exp)
	       (make-operation-exp
		value-exp machine labels operations)
	       (make-primitive-exp
		(car value-exp) machine labels))))
      (lambda ()
	(set-contents! target (value-proc))
	(advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))
;; assign命令から目標のレジスタ名（命令の第二要素）と値の式（命令を形成する式のリストの残り）を取り出す

;; pcを次にすすめる
(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;; * test,branchおよびgoto命令
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
	(let ((condition-proc
	       (make-operation-exp
		condition machine labels operations)))
	  (lambda ()
	    (set-contents! flag (condition-proc))
	    (advance-pc pc)))
	(error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
	(let ((insts
	       (lookup-label labels (label-exp-label dest))))
	  (lambda ()
	    (if (get-contents flag)
		(set-contents! pc insts)
		(advance-pc pc))))
	(error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
	   (let ((insts
		  (lookup-label labels
				(lookup-exp-label dest))))
	     (lambda () (set-contents! pc insts))))
	  ((register-exp? dest)
	   (let ((reg
		  (get-register machine
				(register-exp-reg dest))))
	     (lambda ()
	       (set-contents! pc (get-contents reg)))))
	  (else (error "Bad GOTO instruction -- ASSEMBLE"
		       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; * その他の命令
;; スタック命令saveとrestoreは支持されたレジスタでスタックを使いpcを進めるためだけである
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

;; make-performが扱う命令の最後の型は、実行すべき働きに対して実行手続きを生成する
;; シミュレーション時に働きの手続きが実行され、pcが進められる
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operations-exp? action)
	(let ((action-proc
	       (make-operation-exp
		action machine labels operations)))
	  (lambda ()
	    (action-proc)
	    (advance-pc pc)))
	(error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

;; * 部分式の実行手続き
;; reg, label,constなどでレジスタに代入や演算で使う値を計算する
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
	 (let ((c (constant-exp-value exp)))
	   (lambda () c)))
	((label-exp? exp)
	 (let ((insts
		(lookup-label labels
			      (label-exp-label exp))))
	   (lambda () insts)))
	((register-exp? exp)
	 (let ((r (get-register machine
				register-exp-reg exp)))
	   (lambda () (get-contents r))))
	(else
	 (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

;; 演算式の手続きを命令から作る
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
	(aprocs
	 (map (lambda (e)
		(make-primitive-exp e machine labels))
	      (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol? operations)))
    (if val
	(cadr val)
	(error "Unknown operation -- ASSEMBLE" symbol))))

;; 問題5.9
;; 演算は、レジスタと定数にだけ使えるように条件をつけよ
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
	(aprocs
	 (map (lambda (e)
		(if (label-exp? e) ;; registerとconstでない(label)ならエラー
		    (error "Operation use only register or const")
		    (make-primitive-exp e machine labels)))
	      (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

;; 問題5.10
;; 新しい構文を追加せよ


;; 問題5.11
;; a. Fibonacci計算から1命令を除去できることを示せ
;; http://d.hatena.ne.jp/rsakamot/20090723/1248328615

;; -- before --
;; afterfib-n-2
;;     (assign n (reg val))
;;     (restore val)
;; nにvalが入って、valにスタックトップの値が入る
;; -- after --
;; afterfib-n-2
;;     (restore n)
;; valはそのまま（上でnに入ってた値）、nにスタックトップが入る
;; nとvalの値が逆になるが、命令数が一個減る

;; b. saveしたレジスタへのrestore以外はエラーになるように修正せよ
(define (make-save inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
       (let ((reg (get-register machine reg-name)))
            (lambda ()
                    (push stack (cons reg-name (get-contents reg)))
                    (advance-pc pc)))))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (cons reg (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (let ((head-of-stack (pop stack)))
	(if (eq? (car head-of-stack) reg)
	    (set-contents! reg (cdr head-of-stack))
	    (error "Wrong register name -- RESTORE" reg))
	(advance-pc pc)))))

;; 問題5.12
;; 次の情報を計算機モデルに格納するようにせよ
;; assign, gotoなどの命令の型で、格納されたすべての命令のリスト

;; 入り口を保持するのに使ったレジスタのリスト（goto命令の参照するレジスタ）

;; save,restoreされるレジスタのリスト

;; 各レジスタに対して、代入源のリスト


;; 問題5.13
;; make-machineの引数にレジスタリストを要求せずに、命令列から使うレジスタを決められるようにシミュレータを修正せよ

(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence '()))
    (let ((the-ops
	   (list (list 'initialize-stack
		       (lambda () (stack initialize)))))
	  (register-table
	   (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
	(if (assoc name register-table)
	    (error "Multiply defined register:" name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
	      ;; レジスタが見つからなかったら、エラーにしないで新規レジスタを作成する
	      (begin (allocate-register name)
		     (lookup-register name)))))
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (begin
		((instruction-execution-proc (car insts)))
		(execute)))))
      (define (dispatch message)
	(cond ((eq? message 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      ((eq? message 'install-instruction-sequence)
	       (lambda (seq) (set! the-instruction-sequence seq)))
	      ((eq? message 'allocate-register) allocate-register)
	      ((eq? message 'get-register) lookup-register)
	      ((eq? message 'install-operations)
	       (lambda (ops) (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack) stack)
	      ((eq? message 'operations) the-ops)
	      (else (error "Unknown request -- MACHINE" message))))
      dispatch)))
