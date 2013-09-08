;; 問題2.60
; 重複ありの集合の表現

; 重複なしと一緒
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

; 重複なしと一緒
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set1 set2))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (cons (car set1) (union-set (cdr set1) set2)))))

(define set1 '(1 1 2 3 4 5))
(define set2 '(3 4 5 5 6 6 7))

(element-of-set? 1 set1)
(element-of-set? 0 set1)

(adjoin-set 10 set1)
(adjoin-set 10 '(10 11 12))

(intersection-set set1 set2)

(union-set set1 set2)

; 効率について
; adjoin-set, union-setは重複ありのほうが早い
; 重複なしと比べて，集合の要素数が増えるためelement-of-set?, intersection-setは遅くなる
