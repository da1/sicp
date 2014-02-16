;; 問題3.63
(load "./ch3/3-5-3_Exploiting_the_Stream_Paradigm.scm")

(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))
;; ストリームを最初から構成することになって効率が悪い
;; メモ化しないなら一緒

