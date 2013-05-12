;; 問題1.6
;; Alyssa P. Hackerはifが特殊形式である理由がわからない．
;; condを使って普通の手続きとして提議してはいけないのか？
;; Eva Lu ator は，ifの新版を提議した．

;; then-clause, else-clause共に評価してから判定をしてしまう
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

;; else-clauseに再帰があるため無限ループに陥る
;; 特殊形式でないと，ifの評価前に引数の評価をするために無限ループになる
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
		     x)))

;; 参考にした
;; http://g000001.cddddr.org/1177591058
(define (foo x)
  (if (= x 0)
      "zero"
      (print "foo")))
(define (bar x)
  (new-if (= x 0)
	  "zero"
	  (print "foo")))

(foo 0)
(bar 0)
