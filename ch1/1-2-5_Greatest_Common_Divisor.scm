;; 1.2.5 最大公約数
;; Euclidのアルゴリズム
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; 問題 1.20
(define remainder-old remainder)

(define (remainder a b)
  (print "remainder " a " " b)
  (remainder-old a b))

(gcd 206 40)
