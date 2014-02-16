;; 問題3.57
;; n 番目のfibsを計算するときの加算の実行回数を示せ

;; メモ化してあれば，n番目を計算するのに，n-1番目に１回足すだけ．n回で済む
;; メモ化なしだと，ストリームを作るたびに計算しなおすので大量に計算する必要がある．
(define-macro (delay x) `(lambda () ,x))
(define (force x) (x))

(load "./ch3/3-5-2_Infinite_Streams.scm")

(define count 0)
(define (reset) (set! count 0))
(define (add x y) (begin (set! count (+ count 1)) (+ x y)))

(define (add-streams s1 s2)
  (stream-map add s1 s2))
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (show-fib n)
    (reset)
    (print (stream-ref fibs n))
    (print count)
    'done)

(show-fib 10)

