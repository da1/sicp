;; 4章 超言語的抽象
;; 新しい言語を創設する超言語的抽象（metalinguistic abstraction）は、工学的設計のすべての分野で重要な役割を果す
;;プログラム言語での式の意味を決定する評価器は、もう一つのプログラムに過ぎない
;;ほとんどのプログラムは、ある言語の評価器と見ることができる

;; 我々がやろうとしていることに対して既存の言語では不十分であることがでてくる
;; 常に新しい言語を求めている。
;; 新しい言語を作ってしまおう

;;他の言語を使って言語を創設する技法の度に出発しよう
;;Lispを使い、Lisp自身の評価器を作る
;;LispのScheme方言の部分集合を扱う

;;4.1 超循環評価器
;;評価する言語と同じ言語で書いてある評価器は、超循環（metacircular）である、という。

;;超循環評価器は、本質的に3.2節の評価の環境モデルのSchemeによる形式化である。
;;そのモデルには二つの基本部分があったことを思い出そう

;;1. 特殊形式意外の合成式である組み合わせを評価するには、部分式を評価し、演算子の部分式の値を、非演算子の部分式の値に作用させる
;;2. 合成手続きを一組の引数に作用させるには、手続き本体を新しい環境で評価する。この環境を構成するには、手続きオブジェクトの環境部分を、手続きの仮パラメータが、手続きを作用させる引数に束縛されるフレームで拡張する

;;4.1.1 評価器の中核
;;評価プロセスは、二つの手続きevalとapplyの間の相互作用として記述できる

;; evalは引数として式と環境をとる。
;; 式の型の決定を抽象的に表現し、式の型の特定の表現には関与しない

;;基本式
;;数値のような自己評価式について、evalは式それ自身を返す
;;evalは値を得るため、環境で変数を探す必要がある
;;特殊形式
;;クオート式に対して、evalはクオートされている式を返す
;;変数への代入は,環境に対応付ける新しい値を計算するため、evalを再帰的に呼び出す必要がある。変数の束縛を修正（または作りだし）して環境を修正する
;;if式は、術後が真なら帰結式を評価し、そうでなければ代替式を評価するよう、要素式nの特別な処理を必要とする
;;lambdaはlambda式が指定したパラメータと本体を、評価と環境とともに詰め合わせ、作用可能な手続きへ変換する必要がある
;;begin式は要素式の並びを現れる順に評価する必要がある
;;condによる場合分けは、if式の入れ子に変換してから評価する
;;組み合わせ
;;手続き作用に対して、evalは組み合わせの演算子部分と非演算子部分を再帰的に評価する必要がある。結果の手続きと引数はapplyに渡されそれが実際の作用を扱う


(define true #t)
(define false #f)
;; schemeのapply手続きを退避させる
(define apply-in-underlying-scheme apply)
;;applyの定義 evalでapplyを定義する前にapplyを定義しないと悲しいことになる。
(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
	(else
	 (error
	  "Unknown procedure type -- APPLY" procedure))))

;;evalの定義
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((unbind? exp) (eval-unbinding exp env))
	((if? exp) (eval-if exp env))
	((and? exp) (eval-and exp env))
	((or? exp) (eval-or exp env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((let? exp) (eval (let->combination exp) env))
	((let*? exp) (eval (let*->nested-lets exp) env))
	((letrec? exp) (eval (letrec->let exp) env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((application? exp)
	 (my-apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

;; condを使って実装した。
;; 欠点として、識別可能な型しか扱えず、evalの定義を編集しないと新しい型が追加できない
;; lispのほとんどの実装では、式の型による振り分けはsデータ手動の流儀で行われている。
;; これにより、利用者はevalの定義を修正せずにevalに認識可能な式の新しい型を追加することができる 問題4.3

;;手続きの引数
;;手続きを作用させる引数のリストを作り出す
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

;;条件式
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;並び
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
	(else (eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

;;代入と定義
;;代入する値を見つけるためにevalを呼び出し、変数と結果の値をset-variable-value!に渡し、指示した環境に設定させる
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok)
;;変数の定義も同様に扱う
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;;問題4.1
;; 左から右に式を評価するか、右から左に式を評価するかはSchemeの実装依存である
;; Schemeと無関係に、左から右へと評価されるlist-of-values、右から左へ評価されるlist-of-valuesを書け
;;(define val 10)
;; (define expression '((set! val (+ val 2)) (set! val (* val 2))))
;; (define (list-of-values-left-to-right exps)
;;   (if (null? exps)
;;       '()
;;       (let ((first-eval (eval (car exps) (interaction-environment))))
;;            (cons first-eval
;;                  (list-of-values-left-to-right (cdr exps))))))
;; (list-of-values-left-to-right expression)

;; (define (list-of-values-right-to-left exps)
;;   (if (null? exps)
;;       '()
;;       (let ((first-eval (list-of-values-right-to-left (cdr exps))))
;;            (cons (eval (car exps) (interaction-environment))
;;                  first-eval))))

;; (list-of-values-right-to-left expression)

;; 被演算子を左から右へ評価する list-of-values
;; (define (list-of-values exps env)
;;   (if (no-operands? exp)
;;       '()
;;       (let ((first-eval (eval (first-operand exp) env)))
;; 	(cons first-eval
;; 	      (list-of-values (rest-operands exps) env)))))

;; 被演算子を右から左へ評価する list-of-values
;; (define (list-of-values exps env)
;;   (if (no-operands? exp)
;;       '()
;;       (let ((first-eval (list-of-values (rest-operands) env)))
;; 	(cons (eval (first-exp exp) env)
;; 	      first-eval))))


;;4.1.2 式の表現
;;自己評価式 式と文字列だけ
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
	((string? exp) #t)
	(else #f)))

;;変数は記号で表現する
(define (variable? exp) (symbol? exp))

;;クオート式
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

;;;;指定した記号で始まるリストを識別する手続き
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;;代入
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;;定義
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp))) ;;caadr -> cdrのcarのcar

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ;;仮パラメータ
		   (cddr exp)))) ;;本体

;;lambda式
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;条件式
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

;;;;cond用
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (application? exp) (pair? exp))
(define (opera exp) (car exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;;導出された式
;;condはifの入れ子で定義できる
;;評価プロセスを明確に規程しなければならない特殊形式の数がへるので、評価器が単純になる
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      '#f
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF" clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))

;;condのような、構文変換によって実装するようにした式を導出された式（derived expressions）という。let式もまた、導出された式である

;;問題4.2
;;Louis Reasoner
;;eval内のcond節のなかで、代入よりも手続き作用を先に持ってきたほうが効率的だ

;;a. Louisの計画の何が悪いか
; (define x 3) が、define手続きにxと3を引数として与えると解釈されてしまう
; ペアなら何でも手続きであると言うように実装しているため、applicationの優先順位が高すぎると、色々手続きだと解釈してしまってうまくいかなくなる。

;b. callで手続き作用が始まるようにする
;; 順番が変わっただけ
;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;;         ((variable? exp) (lookup-variable-value exp env))
;;         ((quoted? exp) (text-of-quotation exp))
;;         ((application? exp)
;;          (apply (eval (operator exp) env)
;;                 (list-of-values (operands exp) env)))
;;         ((assignment? exp) (eval-assignment exp env))
;;         ((definition? exp) (eval-definition exp env))
;;         ((if? exp) (eval-if exp env))
;;         ((lambda? exp)
;;          (make-procedure (lambda-parameters exp)
;;                          (lambda-body exp)
;;                          env))
;;         ((begin? exp)
;;          (eval-sequence (begin-actions exp) env))
;;         ((cond? exp) (eval (cond->if exp) env))
;;         (else
;;           (error "Unknown expression type -- EVAL" exp))))

;; (define (application? exp) (tagged-list? exp 'call))
;; (define (operator exp) (cadr exp))
;; (define (operands exp) (cddr exp))

;; (define primitive-procedures
;;   (list (list 'car car)
;;         (list 'cdr cdr)
;;         (list 'cons cons)
;;         (list 'null? null?)
;;         (list '+ +) ;; + を追加
;;         ;; 基本手続きが続く
;;         ))

;;問題4.3
;;;; eval の定義
;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;; 	((variable? exp) (lookup-variable-value exp env))
;; 	(else
;; 	 (if (get 'eval (operator exp))
;; 	     ((get 'eval (operator exp)) exp env)
;; 	     (if (application? exp)
;; 		 (apply (eval (operator exp) env)
;; 			(list-of-values (operands exp) env)))))))

;;;; パッケージのインストール
;; (define (install-eval-package)
;;   (define (text-of-quotation exp env) (cadr exp))
;;   (define (eval-assignment exp env)
;;     (set-variable-value! (assignment-variable exp)
;;                          (eval (assignment-value exp) env)
;;                          env)
;;     'ok)
;;   (define (assignment-variable exp) (cadr exp))
;;   (define (assignment-value exp) (caddr exp))
;;   (define (eval-difinition exp env)
;;     (define-variable! (definition-variable exp)
;;                       (eval (definition-value exp) env)
;;                       env)
;;     'ok)
;;   (define (definition-variable exp)
;;     (if (symbol? (cadr exp))
;;         (cadr exp)
;;         (caadr exp)))
;;   (define (definition-value exp)
;;     (if (symbol? (cadr exp))
;;         (caddr exp)
;;         (make-lambda (cdadr exp)      ; 仮パラメタ
;;                      (cddr exp))))    ; 本体
;;   (define (lambda-parameters exp) (cadr exp))
;;   (define (lambda-body exp) (cddr exp))
;;   (define (make-lambda parameters body)
;;     (cons 'lambda (cons parameters body)))
;;   (define (eval-if exp env)
;;     (if (true? (eval (if-predicate exp) env))
;;         (eval (if-consequent exp) env)
;;         (eval (if-alternative exp) env)))
;;   (define (if-predicate exp) (cadr exp))
;;   (define (if-consequent exp) (caddr exp))
;;   (define (if-alternative exp)
;;     (if (not (null? (cdddr exp)))
;;         (cadddr exp)
;;         #f))
;;   (define (make-if predicate consequent alternative)
;;     (list 'if predicate consequent alternative))
;;   (define (begin-actions exp) (cdr exp))
;;   (define (sequence->exp seq)
;;     (cond ((null? seq) seq)
;;           ((last-exp? seq) (first-exp seq))
;;           (else (make-begin seq))))
;;   (define (make-begin seq) (cons 'begin seq))
;;   (define (cond-clauses exp) (cdr exp))
;;   (define (cond-else-clause? clause)
;;     (eq? (cond-predicate clause) 'else))
;;   (define (cond-predicate clause) (car clause))
;;   (define (cond-actions clause) (cdr clause))
;;   (define (cond->if exp)
;;     (expand-clauses (cond-clauses exp)))
;;   (define (expand-clauses clauses)
;;     (if (null? clauses)
;;         #f
;;         (let ((first (car clauses))
;;               (rest (cdr clauses)))
;;              (if (cond-else-clause? first)
;;                  (if (null? rest)
;;                      (sequence->exp (cond-actions first))
;;                      (error "ELSE clause isn't last -- COND->IF"
;;                             clauses))
;;                  (make-if (cond-predicate first)
;;                           (sequence->exp (cond-actions first))
;;                           (expand-clauses rest))))))
;;   (put 'eval 'quote text-of-quotation)
;;   (put 'eval 'set! eval-assignment)
;;   (put 'eval 'define eval-difinition)
;;   (put 'eval 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp)
;;                                                 (lambda-body exp)
;;                                                 env)))
;;   (put 'eval 'if eval-if)
;;   (put 'eval 'begin (lambda(exp env) (eval-sequence (begin-actions exp) env)))
;;   (put 'eval 'cond (lambda(exp env) (eval (cond->if exp) env)))
;;   'done)

;; (install-eval-package)

;; 問題4.4
;;eval-andとeval-orをつくる。
;;andとorを導出された式として評価する方法を示せ
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (eval-and exps env)
  (define (eval-exp s-exps env)
    (if (true? (eval (car s-exps) env))
	(if (null? (cdr s-exps))
	    (eval (car s-exps) env)
	    (eval-exp (cdr s-exps) env))
	#f))
  (if (null? exps)
      #t
      (eval-exp (cdr exps) env)))

(define (eval-or exps env)
  (define (eval-exp s-exps env)
    (if (null? s-exps)
	#f
	(if (true? (eval (car s-exps) env))
	    (eval (car s-exps) env)
	    (eval-exp (cdr s-exps) env))))
  (if (null? exps)
      #f
      (eval-exp (cdr exps) env)))

; (and '1 '2)
; (and '2 false)
; (or '1 '2)
; (or false '1)

;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;;         ; 省略
;;         ((and? exp) (eval (and->if exp) env))
;;         ((or? exp) (eval (or->if exp) env))
;;         ; 省略
;;         (else
;;           (error "Unknown expression type -- EVAL" exp))))

;;;; and
;; (define (and-clauses exp) (cdr exp))
;; (define (and-first-exp exp) (car exp))
;; (define (and-rest-exps exp) (cdr exp))
;; (define (and->if exp)
;;   (expand-and-clauses (and-clauses exp)))
;; (define (expand-and-clauses clauses)
;;   (define (expand-and-iter clauses result)
;;     (if (null? clauses)
;;         result
;;         (let ((first (and-first-exp clauses))
;;               (rest (and-rest-exps clauses)))
;;              (make-if first
;;                  (expand-and-iter rest first)
;;                  'false))))
;;   (if (null? clauses)
;;       'true
;;       (expand-and-iter clauses '())))

;;;; or
;; (define (or-clauses exp) (cdr exp))
;; (define (or-first-exp exp) (car exp))
;; (define (or-rest-exps exp) (cdr exp))
;; (define (or->if exp)
;;   (expand-or-clauses (or-clauses exp)))
;; (define (expand-or-clauses clauses)
;;   (if (null? clauses)
;;       'false
;;       (let ((first (or-first-exp clauses))
;;             (rest (or-rest-exps clauses)))
;;            (make-if first
;;                     first
;;                     (expand-or-clauses rest)))))

;; 問題4.5
; cond節の構文(<test> => <recipient>)を使えるようにcondの処理を修正せよ
;(cond ((assoc 'b '((a 1) (b 2))) => cadr)
;      (else #f))
;は2を返す
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (cond-expand-cond-clause clause)
  (eq? (cond-predicate clause) '=>))
(define (expand-clauses clauses)
  (if (null? clauses)
      '#f
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF" clauses))
	    (make-if (cond-predicate first)
		     (let ((action (cond-actions first))
			   (predicate (cond-predicate first)))
		       (if (eq? (car action) '=>)
			   (list (cadr action) predicate)
			   (sequence->exp action)))
		     (expand-clauses rest))))))

;(cond ((assoc 'b '((a 1) (b 2))) => cadr) (else false))

;副作用の式をtest にいれると？
; 2回評価されてしまう、schemeの仕様と違う。; lambdaを作って与える、引数が一回だけ評価されるようにせよ

;; 問題4.6
;; letを等価な構文に変換させる
(define (let? exp) (tagged-list? exp 'let))
(define (let-clauses exp) (cdr exp))
(define (let-bindings clause) (car clause))
(define (let-body clause) (cdr clause))

(define (let->combination exp)
  (expand-let-clauses (let-clauses exp)))
(define (expand-let-clauses clause)
  (if (null? (let-bindings clause))
      '()
      (cons (make-lambda (map car (let-bindings clause)) (let-body clause))
	    (map cadr (let-bindings clause)))))

; (define (foo x) (let ((a 2) (b 5)) (+ (* x a) b)))
; (foo 3)

; letが空の時の挙動
; (let () (+ 1 2))

;; 問題4.7
;;;; let*
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-clauses exp) (cdr exp))
(define (let*-bindings clauses) (car clauses))
(define (let*-body clauses) (cadr clauses))
(define (make-let* defs body)
  (list 'let defs body))

(define (let*->nested-lets exp)
  (if (null? exp)
      '#f
      (let ((clauses (let*-clauses exp)))
	(let ((bindings (let*-bindings clauses))
	      (body (let*-body clauses)))
	  (define (iter rest-bindings)
	    (if (null? rest-bindings)
		body
		(make-let* (list (car rest-bindings))
				 (iter (cdr rest-bindings)))))
	  (iter bindings)))))

;(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))

;; 問題4.8
; 名前つきlet
(define (let->combination exp)
  (if (pair? (car (let-clauses exp)))
      (expand-let-clauses (let-clauses exp))
      (expand-named-let-clauses (let-clauses exp))))

(define (named-let-var clauses) (car clauses))
(define (named-let-bindings clauses) (cadr clauses))
(define (named-let-body clauses) (caddr clauses))
(define (expand-named-let-clauses clauses)
  (make-begin
   (list
    (list 'define (cons (named-let-var clauses)
			(map car (named-let-bindings clauses)))
	  (named-let-body clauses))
    (cons (named-let-var clauses)
	  (map cadr (named-let-bindings clauses))))))

; (define (fib n) (let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))

;; 4.1.3 評価器のデータ構造
;; 述語のテスト
(define (true? x)
  (not (eq? x #f)))
(define (false? x)
  (eq? x #f))

;; 手続きの表現
;(apply-primitive-procedure <proc> <args>)
; 与えられた基本手続きをリスト<args>にある引数の値に作用させ、作用の結果を返す

; (primitive-procedure? <proc>)
; procが基本手続きかどうかをテストする
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; 環境に対する操作
; (lookup-variable-value <var> <env>)
;環境のなかで記号に束縛された値を返す、未束縛ならエラー

;(extend-environment <variables> <values> <base-env>)
;外側の環境を環境<base-env>とし、リスト<variables>の記号がリスト<values>の対応する要求に束縛された新しいフレームからなる新しい環境を返す

;(define-variable! <var> <value> <env>)
;環境の最初のフレームに変数<var>と値<value>を対応付ける新しい束縛を追加する

;(set-variable-value! <var> <value> <env>)
;環境の変数の束縛を変更し、その変数が値に束縛されるようにするか、変数が未束縛ならエラーにする

;環境をフレームのリストとして表現する。ある環境の外側の環境はリストのcdrである
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; 環境の各フレームは、リストの対：そのフレームで束縛されている変数のリストと、対応付けられている値のリストで表現する
(define (make-frame variable values)
  (cons variable values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;変数を値に対応付ける新しいフレームで、環境を拡張するには、変数のリストと値のリストからなるフレームを作り
;これを環境に接続する。変数の個数が値の個数に一致しなければエラー
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few argument supplied" vars vals))))

;環境の中で変数を探すには、最初のフレームで変数のリストを走査する
;探している変数が見つかれば値リストの対応する要素を返す。現在のフレームに変数紙浸からなければ外側の環境を探しこれを続ける。
;空の環境に達したら、未束縛変数エラーを出す
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))
;指定された環境のなかで変数を新しい値に設定するには、lookup-variable-valuesのように変数を走査し、それが見つかれば対応する値を変更する
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var  (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

;変数を定義するには、最初のフレームで変数の束縛を探し、それが存在すれば束縛を変更する。束縛が泣ければ最初のフレームに接続する
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

;;4.1.4 評価器をプログラムとして走らせる
;一義的なオブジェクトを、評価しようとしている式に現れ得る基本手続きの名前に対応付ける大域的環境を設定する
;帯域的環境には、また、評価する式で変数として使えるように記号trueとfalseの束縛もある
(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

;setup-environmentは基本手続きの名前と実装手続きをリストからとる
;http://www.unixuser.org/~euske/doc/r5rs-ja/r5rs-ja.pdf
(define primitive-procedures
  (list (list 'car car)
    (list 'cdr cdr)
    (list 'cons cons)
    (list 'null? null?)
    (list 'eqv? eqv?)
    (list 'eq? eq?)
    (list 'equal? equal?)
    (list 'assoc assoc)
    (list 'cadr cadr)
    (list '+ +)
    (list '- -)
    (list '* *)
    (list '/ /)
    (list '= =)
    (list 'print print)
    (list 'list list)
    (list 'memq memq)
    (list 'member member)
    (list 'not not)
    (list '> >)
    (list '< <)
    (list '>= >=)
    (list 'abs abs)
    (list 'remainder remainder)
    (list 'integer? integer?)
    (list 'sqrt sqrt)
))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;;基盤のLispシステムの読み込み、評価、印字ループをモデル化する駆動ループ（driver loop）を用意する
;促進記号を印字し、入力式を読み込みこの式を大域環境で評価し、結果を印字する
;印字された結果の前に出力記号をつけ、式の値を他の印字された出力から区別できるようにする

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))
;; (define the-global-environment (setup-environment))
;; (driver-loop)

; (define (append x y) (if (null? x) y (cons (car x) (append (cdr x) y))))
; (append '(a b c) '(d e f))


;;問題4.11
;;フレームをリストの対で表現する代わりに、各束縛が名前値の対であるような束縛のリストでフレームを表現することができる
;; 名前のリストと値のリストのconsから名前と値の対のリストにする
;; (define (make-frame variables values)
;;   (define (make-frame-iter variables values)
;;     (if (null? variables)
;; 	'()
;; 	(cons (cons (car variables)
;; 		    (car values))
;; 	      (make-frame-iter (cdr variables)
;; 			       (cdr values)))))
;;   (make-frame-iter variables values))

;; (define (frame-variables frame)
;;   (if (null? frame)
;;       '()
;;       (cons (caar frame)
;; 	    (frame-variables (cdr frame)))))

;; (define (frame-values frame)
;;   (if (null? frame)
;;       '()
;;       (cons (cdar frame)
;; 	    (frame-values (cdr frame)))))

;; (define (add-binding-to-frame! var val frame)
;;   (set-cdr! frame (cons (cons var val) (cdr frame))))

;; 問題4.12
;;set-variable-value! define-variable! lookup-variable-value
;;環境構造を渡り歩くより抽象的な手続きを使って表すことができる。共通パターンを取り込む抽象を定義し、その抽象を使って三つの手続きを再定義せよ。
;; scanを抽象化する
;; (define (scan var vars vals)
;;   (cond ((null? vars) '())
;; 	((eq? var (car vars)) vals)
;; 	(else
;; 	 (scan var (cdr vars) (cdr vals)))))

;; (define (lookup-variable-value var env)
;;   (define (env-loop env)
;;     (if (eq? env the-empty-environment)
;; 	(error "Unbound variable" var)
;; 	(let ((frame (first-frame env)))
;; 	  (let ((result-of-scan (scan var
;; 				      (frame-variables frame)
;; 				      (frame-values frame))))
;; 	    (if (null? result-of-scan)
;; 		(env-loop (enclosing-environment env))
;; 		(car result-of-scan))))))
;;   (env-loop env))

;; (define (set-variable-value! var val env)
;;   (define (env-loop env)
;;     (if (eq? env the-empty-environment)
;; 	(error "Unbound variable -- SET!" var)
;; 	(let ((frame (first-frame env)))
;; 	  (let ((result-of-scan (scan var
;; 				      (frame-variables frame)
;; 				      (frame-variables frame))))
;; 	    (if (null? result-of-scan)
;; 		(env-loop (enclosing-environment env))
;; 		(set-car! result-of-scan val))))))
;;   (env-loop env))

;; (define (define-variable! var val env)
;;   (let ((frame (first-frame env)))
;;     (let ((result-of-scan (scan var (frame-variables frame) (frame-values frame))))
;;       (if (null? result-of-scan)
;; 	  (add-binding-to-frame! var val frame)
;; 	  (set-car! result-of-scan val)))))

;; env-loop を取り出して、変数が見つかったときと見つからなかったときの手続きを引数として渡す。というやり方

;; 問題4.13
;; schemeではdefineにより変数の新しい束縛をつくることができる。
;; 束縛を除去する方法がない
;; unbind!式、評価した環境から与えられた記号の束縛を除去する特殊形式を実装せよ

;; 現在のフレームからだけ削除する。変数の参照先を変える
(define (unbind? exp) (tagged-list? exp 'unbind!))

(define (unbinding-varialbe exp) (cadr exp))
(define (eval-unbinding exp env)
  (unbind-variable! (unbinding-varialbe exp) env)
  'ok)

(define (unbind-variable! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (error "Unbound variabl --UNBIND-VARIABLE:" var))
	    ((eq? var (car vars))
	     (set-car! vars (cadr vars))
	     (set-cdr! vars (cddr vars))
	     (set-car! vals (cadr vals))
	     (set-cdr! vals (cddr vals)))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

;;
; (define x 'a)
; (define (foo y) (define x 'b) (print (cons x y)))
; (foo 'a)
; (b . a)
; (unbind! x)
; (foo 'c)
; (b . c)

;; 問題4.14
;Eva Lu AtorとLouis Reasoner
;Eva -> mapの定義を入力した。
;Louis -> mapのシステム版を超循環評価機の基本手続きとして組み込んだ
;Louisが失敗した理由を説明せよ
; Schemeのapplyが、頭についた'premitiveを評価しようとしている、ので失敗する
; applyの定義を追っていけばわかる

; 4.1.5 プログラムとしてのデータ
; Lisp式を評価するLispプログラムを考えるには、類推が有用かもしれない。

; 評価機を機械の記述として考える。入力が与えられると評価機は記述された機械をエミュレートするように自分を構成する
; 評価機にfacorialの定義を与えると評価機は階乗が計算できるようになる。

; 我々の機械は、万能機械（universal machine）と見てよい。
; 評価機は、プログラム言語で操作されるデータオブジェクトと、プログラム言語自身の間の橋として働くこと
; 評価機のプログラムが走っていて、利用者が評価機に式を入力して結果を見ているとして、
; 利用者視点では (* x x)のような式は評価機我実行すべきプログラム言語での式である
; 評価器視点では、単なるリストである
; 式(* x x)とデータ構造(* x x)の橋渡しを評価機がしている
; (eval '(* 5 5) user-initial-environment)
; (eval (cons '* (list 5 5)) user-initial-environment)
; は同じ結果を返す

;; 問題4.15
; 1引数手続きpとオブジェクトaが与えられ、(p a)の評価が値を返すならpはaで停止するという
; 任意の手続きpとオブジェクトaに対して、pがaで停止するかどうか正しく決める手続きを書くことは不可能であることを示せ

;そういう手続きhalts?があるなら次のようなプログラムが実装できる
;; (define (run-forever) (run-forever))
;; (define (try p)
;;   (if (halts? p p)
;;       (run-forever)
;;       'halts?))
; (try try)の評価を考え、可能な結果がhalts?の意図した結果と矛盾することを示せ
; 停止するならhalts?は真、停止しないならhalts?は偽を返すとすると、
; (halts? try try) が真なら、(run-forever)が走るため、tryは停止しない
; (halts? try try) が偽なら、(try try)は停止する。
; halts?の結果とプログラムの結果が矛盾する。

; 停止性問題

;; 4.1.6 内部定義
;; ブロック構造を実装するのに使う内部定義について、注意深く考えてみると、名前ごとの環境の拡張は局所変数を定義する最良の方法とは思えない。
;; 互いに再帰的な手続き
;; 一般には、ブロック構造において、局所的な名前の有効範囲は、defineが評価される手続き本体全体である。

;; 内部で定義した名前が真に同時有効範囲を持つように定義を扱う単純な方法がある。
;; 現在の環境にあるすべての変数をその値の式を評価する前に作り出してしまう。
;; lambda式の本体を評価する前に本体にある内部定義を吐き出し消してしまう。

;; 問題4.16
;; a. lookup-variable-valueを変更して、見つけた値が*unassigned*ならエラーとなるようにした
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (if (eq? '*unassigned* (car vals))
		 (error "Unassigned variable -- LOOKUP-VARIABLE-VALUE" var)
		 (car vals)))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

;; b. 手続き本体をとり、上に述べた変換を施すことで、内部定義のない等価なものを返す手続きscan-out-definesを書け
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

;; definitionの式、そうでない式、definitionの式という構成になっているとシンタックスエラーになるかも

;; 問題4.17
;; 定義を逐次的に解釈する時にどう構造化されるか。定義を吐き出した場合に構造化されるかの違いを比べよ。
;; 環境の図を書け
;; 変換したプログラムに余計なフレームがあるのはなぜか。この違いがプログラムの行動に違いを生じない理由を説明せよ

;; defineがletに置き換えられ、letはlambdaに置き換えられる。
;; lambdaが余計なフレームを作るが、変数の検索には影響がないのでプログラムは変わらない。

;; 余計なフレームを作らずに、せよ。
;; letでなくdefineにしておく
;;

;; 問題 4.18
;; (define (solve f y0 dt)
;;   (define y (integral (delay dy) y0 dt))
;;   (define dy (stream-map f y))
;;   y)

;; これは、以下のようになる。

;; (define solve
;;   (lambda (f y0 dt)
;;     (let ((y '*unassigned*)
;;           (dy '*unassigned*))
;;       (let ((a (integral (delay dy) y0 dt))
;;             (b (dy (stream-map f y))))
;;         (set! y a)
;;         (set! dy b)
;;         y))))

;; a の束縛を作る時に dy が必要となるが、この時点での dy は *unassigned* だ。
;; 同様に b の束縛を作る時に y が必要になっており、これも *unassigned* だ。
;; 当然、 dy, y とも期待する値は *unassigned* ではないので、この手続きは期待した通りに動作しない。

;; これに対して、本文中の定期の掃き出し戦略を考えてみる。

;; (define solve
;;   (lambda (f y0 dt)
;;     (let ((y '*unassigned*)
;;           (dy '*unassigned*))
;;       (set! y (integral (delay dy) y0 dt))
;;       (set! dy (stream-map f y))
;;       y)))

;; 同じように y に値を set! する時点では dy が決まらず、dy に値を set! する時点では y が決まらず、という状態になっている。
;; 超循環評価器での eval を見てみると、set! は eval-assignment として評価される。
;; (set! y (integral (delay dy) y0 dt))だけを考えた場合、
;; assignment-variable は y。assignment-value は (integral (delay dy) y0 dt) だ。
;; これらに基づき eval-assignment された時点では、(integral ...) は '(procedure () (integral ...)というS式になるだけで、実際に integral 手続きが評価されるわけではない。
;; 対して、新しい戦略のようにしてしまうと、let 式で a に (integral ... を束縛しようとした時点で(integral ...) を評価してしまう。当然、 (integral ...) の評価に必要なパラメータ dy が*unassigned* であるため問題発生、となる。

;; 問題4.19
;; Ben Bitdiddle, Alyssa P.Hacker, Eva Lu Ator
;; 3人の意見のどれを支持するか。
;; Evaの望むように振る舞うよう内部定義を実装する方法が考えられるか

;; http://practical-scheme.net/wiliki/wiliki.cgi?Scheme%3A%E5%86%85%E9%83%A8define%E3%81%AE%E8%A9%95%E4%BE%A1%E9%A0%86
;; gausheではAlyssaの方式
;; Evaの方式には、delayをはさめばできる 変数を最初に使うまでdelayする
;; eva式はスコープについて考える幅が減っていいよね

;; 問題4.20
;; letrec
;; (letrec ((<var1> <exp1>) .. (<varn> <expn>)) <body>)
;; letrecはletの変形で変数varkの初期値となる式<expk>は、letrecのすべての束縛を含む環境で評価される。
;;これは、上の例のeven?とodd?の相互再帰のような束縛の再帰や10の階乗の評価を許す

;; a.letrecをlet式に変換して導出した指揮として実装せよ
;; letrecの変数は、letで作り出されその後その値はset!で代入される。
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec->let exp)
  (let ((vars (map car (cadr exp)))
	(exps (map cdr (cadr exp)))
	(body (cddr exp)))
    (cons 'let
	  (cons (map (lambda (x) (list x ''*unassigned*)) vars)
		(append (map (lambda (x y) (cons 'set! (cons x y)))
			     var exps)
			body)))))

;; b.Louis Reasoner
;; 手続きの内側で、defineが使いたくなければletを使うことができる。
;; fが定義されているとして、(f 5)の評価中で、fの本体の残りが評価される環境を示す環境ダイアグラムを書いて示せ
;; fの定義がletrecがletになった場合の環境ダイアグラムを書け
; letをdefineの代わりに使おうとしたら、再帰的な定義ができない

;; 問題4.21
;;a.
;; ((lambda (n)
;;    ((lambda (fact)
;;       (fact fact n))
;;     (lambda (ft k)
;;       (if (= k 1)
;; 	  1
;; 	  (* k (ft ft (- k 1)))))))
;;  10)

;; lambda (n)に10を摘要
;; lambda (fact)に、lambda(ft k)を摘要
;; k * (fact fact (- k 1))の形になって最終的に階乗になる
;; フィボナッチ
;; ((lambda (n)
;;    ((lambda (fact)
;;       (fact fact n))
;;     (lambda (ft k)
;;       (if (= k 0)
;; 	  0
;; 	  (if (= k 1)
;; 	      1
;; 	      (+ (ft ft (- k 1)) (ft ft (- k 2))))))))
;;  10)

;; yコンビネータとかそういう話らしい
;; これが10の階乗を返す

;; b.相互に再帰的な内部定義をもつ次の手続きを考える。
;; (define (f x)
;;   ((lambda (even? odd?)
;;            (even? even? odd? x))
;;    (lambda (ev? od? n)
;;            (if (= n 0) #t (od? ev? od? (- n 1))))
;;    (lambda (ev? od? n)
;;            (if (= n 0) #f (ev? ev? od? (- n 1))))))

