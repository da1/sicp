;; 問題5.16
;; 命令トレースができるように拡張せよ
;; 命令実行前に、命令の文字列を印字する
;; trace-on と trace-offでトレースの開始と停止を制御できるようにせよ
;; http://www.serendip.ws/archives/3278
;; http://himoiku.cocolog-nifty.com/blog/2008/04/sicp515516_4bab.html

(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence '())
    	(instruction-count 0)
	(instruction-sequence '())
	(instruction-trace-flag #f)
        (the-instruction-sequence '()))
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
		(set! instruction-count (+ 1 instruction-count))
		(print-trace (car insts))
		(execute)))))
      (define (get-instruction-count)
	instruction-count)
      (define (initialize-instruction-count)
	(set! instruction-count 0))
      (define (set-instruction-trace flag)
	(set! instruction-trace-flag flag))
      (define (print-trace inst)
	(if instruction-trace-flag
	    (begin
	      (display (instruction-text (car inst)))
	      (newline))))
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
(fact-machine 'trace-off)
(set-register-contents! fact-machine 'n 5)
(start fact-machine)
(get-register-contents fact-machine 'val)
