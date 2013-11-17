;; 3.27
;; memoization
;; フィボナッチ数を計算する手続き
(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

;; memoizationの手続き
;; ここで使うlookupとinsert!は最初に定義したバージョン
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(memo-fib 3)
(define x 10000)
(memo-fib x)
(memo-fib x)

;; これの環境を書け
;; グローバル環境 に手続きmemo-fibとmemoize
;; (memo-fib 3)を実行するときに環境ができる．
;; (let ..)はlambdaを定義して，それを評価している.
;; f lambdaの環境
;; lambdaの環境 ここでmake-tableされる
;; そこからlambda(x)が生えてる
;; さらに生えるlambda xに3が束縛されてる
;; そこから生える環境 let
;; tableのある環境からnが2のとき，1のときの環境が生える

;; 最終的にtableのある環境にいろいろ値が入る

;; たいへんめんどくさいですね

;; 途中経過はメモ化されない


