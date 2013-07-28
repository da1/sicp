;; 問題2.18
;; リストの逆順を返す手続きreverse

(define (reverse a)
  (define (reverse-iter a b)
    (if (= (length a) 1)
      (cons (car a) b)
      (reverse-iter (cdr a) (cons (car a) b))))
  (reverse-iter a ()))

(reverse (list 1 4 9 16 25))
(reverse (list 1))
