;; 問題3.58
(load "./ch3/3-5-2_Infinite_Streams.scm")
;; 次の手続きにより計算されるストリームを解釈せよ
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(show-stream (expand 1 7 10) 1 10)
;; 1 4 2 8 5 7 1 4 ...
(show-stream (expand 1 7 6) 0 10)

(show-stream (expand 3 8 10) 0 10)
;; 3 7 5 0 0 ..

(show-stream (expand 3 8 2) 0 10)

;; 割り算の筆算のように計算される
;; carに商，cdrにあまりを下におろして続きの計算をするイメージ

