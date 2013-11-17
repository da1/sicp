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

