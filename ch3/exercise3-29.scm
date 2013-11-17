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

