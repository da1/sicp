;; 問題3.58
;; 次の手続きにより計算されるストリームを解釈せよ
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(define ex1 (expand 1 7 6))
(show-stream ex1 0 10)
(stream-ref ex1 0)
;; 1 4 2 8 5 7 1 4 ...

(define ex2 (expand 3 8 2))
(show-stream ex2 0 5)
(stream-ref ex2 0)
;; 3 7 5 0 0 ..

;; 割り算の筆算のように計算される
;; carに商，cdrにあまりを下におろして続きの計算をするイメージ

