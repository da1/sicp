;; 問題3.40
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))
;; 1000000 -> *xx,*xxx *xxx,*xxでも同じ
;; 100     -> *xxset前に*xxxがset
;; 1000    -> *xxxset前に*xxがset
;; 10000   -> P1がxを２回評価する間にP2がxを1000に変える, P2がxを２回評価後にP1がxを100に変えたケースも一緒
;; 100000  -> P2がxを１回評価後に,P1がxを100に変更

;;1000000


