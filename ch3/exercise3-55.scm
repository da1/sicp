;; 問題3.55
;; ストリームSをとり，その要素は S0, S0+S1, S0+S1+S2,...
;; となるようなストリームを返す手続きを定義せよ
(load "./ch3/3-5-2_Infinite_Streams.scm")

(define (partial-sums s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s)))))
(define p (partial-sums integers))
(show-stream p 0 10)

