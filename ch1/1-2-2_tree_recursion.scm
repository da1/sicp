; 1.2.2 木構造再帰
(define (fib n)
  (cond ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))))

;; この実装では，ステップ数が指数的に増加する

(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;; こちらの実装は，線形の増加

;; 例：両替の計算

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
    ((or (< amount 0) (= kinds-of-coins 0)) 0)
    (else (+ (cc amount (- kinds-of-coins 1))
         (cc (- amount (first-denomination kinds-of-coins))
             kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
    ((= kinds-of-coins 2) 5)
    ((= kinds-of-coins 3) 10)
    ((= kinds-of-coins 4) 25)
    ((= kinds-of-coins 5) 50)))

(count-change 5)
