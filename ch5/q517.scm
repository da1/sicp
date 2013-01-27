;; 問題5.17
;; ラベルを印字できるようにせよ
(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence '())
    	(instruction-count 0)
	(instruction-sequence '())
	(instruction-trace-flag #f)
        (the-instruction-sequence '())
	(label '()))
    (let ((the-ops
	   (list (list 'initialize-stack
		       (lambda () (stack 'initialize)))
		 (list 'print-stack-statistics
		       (lambda () (stack 'print-stack-statistics)))))
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
		(if (not (eq? (caaar insts) 'label))
		    (set! instruction-count (+ 1 instruction-count))
		    (set! label (cadr (caar insts))))
		(print-trace (car insts) label)
		(execute)))))
      (define (get-instruction-count)
	instruction-count)
      (define (initialize-instruction-count)
	(set! instruction-count 0))
      (define (set-instruction-trace flag)
	(set! instruction-trace-flag flag))
      (define (print-trace inst label)
	(if instruction-trace-flag
	    (begin
              (print "label: " label ", instruction: " (car inst)))
	      (newline)))
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
	      ((eq? message 'get-instruction-count)
	       (let ((cnt (get-instruction-count)))
		 (initialize-instruction-count)
		 cnt))
	      ((eq? message 'initialize-instruction-count)
	       (initialize-instruction-count))
	      ((eq? message 'trace-on) (set-instruction-trace #t))
	      ((eq? message 'trace-off) (set-instruction-trace #f))
	      (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (get-instruction-counting machine)
  (machine 'get-instruction-count))

(define (initialize-instruction-counting machine)
  (machine 'initialize-instruction-count))

;; 命令のリストとラベルを対応付ける表を作る
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
		      (lambda (insts labels)
			(let ((next-inst (car text)))
			  (if (symbol? next-inst)
			      (let ((insts2 (cons (list (list 'label next-inst))
						 insts)))
				;;label
				(receive insts2
					 (cons (make-label-entry next-inst
								 insts2)
					       labels)))
			      ;; 命令
			      (receive (cons (make-instruction next-inst)
					     insts)
				       labels)))))))

(define (make-execution-procedure inst labels machine
				  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
	 (make-assign inst machine labels ops pc))
	((eq? (car inst) 'test)
	 (make-test inst machine labels ops flag pc))
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
	((eq? (car inst) 'label)
	 (lambda () (advance-pc pc)))
	(else (error "Unknown instruction type -- ASSEMBLE" inst))))

(define fact-machine
  (make-machine
   '(n val continue)
   (list (list '* *) (list '= =) (list '- -))
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

(fact-machine 'trace-on)
(set-register-contents! fact-machine 'n 3)
(start fact-machine)
(get-register-contents fact-machine 'val)