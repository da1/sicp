;; 問題3.68
(define (pairs s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (pairs (stream-cdr s) (stream-cdr t))))

;; http://www.serendip.ws/archives/1682
;; cons-streamを使ってないため無限ループになる

