;; 問題2.54
;; 2つのリストが同じ順に並んだ同じ要素を含むときに，equal?である
(equal? '(this is a list) '(this is a list))

; は真

(equal? '(this is a list) '(this (is a) list))

;は偽である

; 基本となるeq?を使い，aとbが記号であって両者がeq?であるか
; あるいは両者が，(car a)が (car b)にequal?であり，(cdr a)が(cdr b)にequal?であるようなリストであると
; equal?を定義できる．これを使ってequal?を実装せよ

(define (equal? a b)
  (cond ((and (list? a) (list? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        (else
          (eq? a b))))

(define (equal? a b)
  (if (and (list? a) (list? b))
    (if (and (null? a) (null? b))
      #t
      (if (or (null? a) (null? b))
        #f
        (and (equal? (car a) (car b))
             (equal? (cdr a) (cdr b)))))
    (eq? a b)))

(equal? 1 1)
(equal? 1 2)
(equal? 'a 'a)
(equal? '(a b) '(a b))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
