;; 問題 1.11
;; f(n) = n ; n < 3
;; f(n) = f(n-1)+2f(n-2)+3f(n-3) n>=3

;; 再帰的プロセス，反復的プロセスでこれを計算する手続きを書け

;; 再帰版
(define (f n)
  (cond ((< n 3) n)
    (else (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3)))))))

(define (print-f n)
  (cond ((>= n 0)
         (display n)
         (display " : ")
         (print (f n))
         (print-f (- n 1)))))

(print-f 7)

;; 反復版
(define (f n)
  (f-iter 2 1 0 n))

(define (f-iter a b c count)
  (if (= count 0)
    c
    (f-iter (+ a (* b 2) (* c 3)) a b (- count 1))))

