;; 問題5.18
;; レジスタがトレースできるようにせよ
;; http://www.serendip.ws/archives/3307

(define (make-register name)
  (let ((contents '*unassigned*)
	(register-trace-flag #f))
    (define (set-register-trace flag)
      (set! register-trace-flag flag))
    (define (dispatch message)
      (cond ((eq? message 'get)
	     (if register-trace-flag
		 (print "get register " name ": " contents))
	     contents)
	    ((eq? message 'set)
	     (lambda (value)
	       (if register-trace-flag
		   (print "set register " name ": " contents " <- " value))
	       (set! contents value)))
	    ((eq? message 'trace-on) (set-register-trace #t))
	    ((eq? message 'trace-off) (set-register-trace #f))
	    (else
	     (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (set-register-trace-flag machine register-name flag)
  ((get-register machine register-name) flag))

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
(set-register-trace-flag fact-machine 'n 'trace-on)
(set-register-trace-flag fact-machine 'val 'trace-on)

(start fact-machine)
(get-register-contents fact-machine 'val)