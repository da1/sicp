;then-clause, else-clause共に評価してから判定をしてしまう
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

;else-clauseに再帰があるため無限ループに陥る
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
		     x)))

; 参考にした
; http://cadr.g.hatena.ne.jp/g000001/20070426/1177591058
(define (foo x)
  (if (= x 0)
      "zero"
      (print "foo")))
(define (bar x)
  (new-if (= x 0)
	  "zero"
	  (print "foo")))
