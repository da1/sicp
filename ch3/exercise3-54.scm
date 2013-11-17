;; 問題3.54
;; add-streamのように，２つのストリームの要素ごとの積を生じる手続きmul-streamsを定義せよ
;; integersストリームを使い，n番目の要素がn+1の階乗になるストリームの定義を完成させよ
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams factorials integers)))
(show-stream factorials 0 10)

