;; 問題2.36

; 手続きaccumulate-n 第三引数に，すべてが同数の要素からなる並びの並びをとる他はaccumulateと同じである
; アキュムレーションとして指定した手続きを，並びのすべての第一要素すべての第二要素というふうに作用させ結果の並びを返す

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(load "./ch2/2-2-3_Sequences_as_Conventional_Interfaces.scm")

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init
                      (map (lambda (x) (car x)) seqs))
          (accumulate-n op init
                        (map (lambda (x) (cdr x)) seqs)))))

(accumulate-n + 0 s)
; (22 26 30)
