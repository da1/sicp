;; 問題 1.16
;; 逐次平方を使い，対数的ステップ数の反復的べき乗プロセスを生成する手続きを設計せよ

(define (fast-expt2 b n)
  (fast-expt-iter b n 1))
(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
    ((even? n) (fast-expt-iter (* b b) (/ n 2) a))
    (else (fast-expt-iter b (- n 1) (* a b)))))
(fast-expt2 3 3)
(= (fast-expt 3 1000) (fast-expt2 3 1000))
