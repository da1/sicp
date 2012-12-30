;; 3.3.4 ディジタル回路のシミュレータ

;; 事象駆動シミュレーション（event-driven simulation）

;; 回線を構成数r手続き
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)

;; 半加算器
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;; 全加算器
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; このシミュレータは，回路の言語を構成する道具を提供する
;; 1.1節でLispの勉強に利用した道具の一般的な視点を採用するなら，
;; 基本的な構造箱は言語の組み込み要素であり，箱の結線は組み合わせの手段を用意することである．
;; 結線のパターンを手続きとして規定するのは，抽象の手段として役立つ

;; 基本的な機能箱
;; (get-signal <wire>)
;; 回線の信号の現在の位置を返す
;; (set-signal! <wire> <new value>)
;; 回線の信号を新しい値に変更する
;; (add-action! <wire> <procedure of no argument>)
;; 回線の信号が値を変えた時，指定した手続きが走ることを主張する

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
	   (logical-and (get-signal a1) (get-signal a2))))
    (after-delay and-gate-delay
		 (lambda ()
		   (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
(define (logical-and a1 a2)
  (* a1 a2))

;; 問題3.28
;; or-gateの実装
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
	   (logical-or (get-signal a1) (get-signal a2))))
    (after-delay or-gate-delay
		 (lambda ()
		   (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
(define (logical-or a1 a2)
  (let ((add (+ a1 a2)))
    (if (< add 1)
	1
	add)))

;; 問題3.29
;; andとinverterで作ったor-gate
;; or = nand(nand(x,x), nand(y,y))
;; nand(x,x) = not(x)
;; or = not and(not(x), not(y))
(define (or-gate a1 a2 output)
  (let ((w1 (make-wire))
	(w2 (make-wire))
	(w3 (make-wire)))
    (inverter a1 w1)
    (inverter a2 w2)
    (and-gate w1 w2 w3)
    (invert w3 output)))
;; 遅延時間
;; (+ inverter-delay and-gate-delay inverter-delay)

;; 問題3.30
(define (ripple-carry-adder list-a list-b list-sum c-out)
  (define (iter list-a list-b list-sum c-in)
    (if (not (null? list-a))
        (let ((c-out (make-wire)))
             (full-adder (car list-a) (car list-b) c-in (car list-sum) c-out)
             (iter (cdr list-a) (cdr list-b) (cdr list-sum) c-out))
        'ok))
  (iter list-a list-b list-sum c-out))

(define a1 (make-wire))
(define a2 (make-wire))
(define b1 (make-wire))
(define b2 (make-wire))
(set-signal! b1 1)
(set-signal! b2 1)
(get-signal b1)
(get-signal b2)
(define s1 (make-wire))
(define s2 (make-wire))
(ripple-carry-adder (list a1 a2) (list b1 b2) (list s1 s2) (make-wire))
(get-signal s1)
(get-signal s2)

(define c (make-wire))
(or-gate a1 b1 c)
(get-signal c)

;; 遅延
;; (* n (FA-delay))
;; (+ HA-delay HA-delay or-gate-delay)
;; (+ and-gate-delay inverter-delay and-gate-delay)
;; n * (4*and-gate-delay + 2*inverter-delay + or-gate-delay)

;; carryの遅延は，sumがもとまるよりも早いことを考慮する

;; 遅延時間が，inverter-delay + and-gate-delay = or-gate-delayのとき，sumとcarryの求まる時間は同じになった

;; 回路の表現
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-value)
		 (call-each action-procedures))
	  'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
	    ((eq? m 'set-signal!) set-my-signal!)
	    ((eq? m 'add-action!) accept-action-procedure!)
	    (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
	((car procedures))
	(call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;; 新しい回線が作り出されると，状態変数の新しい組みが割り当てられ，
;; 環境に新しい状態変数を取り込んだ新しいdispatch手続きが構成され返される

;; 次第書き
;; シミュレータの完成に必要なafter-delay
;; なすべき計画を含む次第書き（agenda）というデータ構造を保持することである．
;; agendaのための演算の定義
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
		  action
		  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
	(first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate))))

;; 先に
;; agendaの実装
;; 各手続きは，時間区分（time segment）でできている．
;; 各時間区分は数値とその時間区分の間に起きるように計画された手続きを保持するキューの対である

(include "queue.scm")

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
	(< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
	(insert-queue! (segment-queue (car segments))
		       action)
	(let ((rest (cdr segments)))
	  (if (belongs-before? rest)
	      (set-cdr!
	       segments
	       (cons (make-new-time-segment time action)
		     (cdr segments)))
	      (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
	(set-segments!
	 agenda
	 (cons (make-new-time-segment time action)
	       segments))
	(add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
	(set-segments! agenda (rest-segments agenda)))))
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
	(set-current-time! agenda (segment-time first-seg))
	(front-queue (segment-queue first-seg)))))

;; シミュレーションの例
;; 回線上にプローブを置く
(define (probe name wire)
  (add-action! wire
	       (lambda ()
		 (newline)
		 (display name)
		 (display " ")
		 (display (current-time the-agenda))
		 (display " New-value = ")
		 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)
(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
(set-signal! input-2 1)
(propagate)

;; procを必ず実行している理由
;; 定義だけして，実行されない

