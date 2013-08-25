;; 問題2.42
; エイトクイーンパズル

(load "./ch2/2-2-3_Sequences_as_Conventional_Interfaces.scm")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; rest-of-queens 最初のk-1列にk-1個のクイーンを置く方法

; new-rowはk列目にクイーンの置ける行の案である

; adjoin-position
; 位置の集合に新しい場所の座標を連結する

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

;(adjoin-position 1 1 '())

; empty-board
; 場所の空集合を表す
(define empty-board '())

; safe?
; 他のクイーンに対してk番目のクイーンが安全な場所かを決める
;; 参考
; http://www.serendip.ws/archives/776

(define (safe? k positions)
  (let ((head (car positions)))
    (define (iter rest)
      (cond ((null? rest) true)
            ((conflicts? (car rest) head) false)
            (else (iter (cdr rest)))))
    (iter (cdr positions))))

(define (conflicts? a b)
  (let ((dx (abs (- (car a) (car b))))
        (dy (abs (- (cadr a) (cadr b)))))
    (cond ((= dx 0) true)
          ((= dy 0) true)
          ((= dx dy) true)
          (else false))))

(define (show-queen rest-of-queens)
  (if (null? rest-of-queens)
    '()
    (begin
      (display (car rest-of-queens))
      (newline)
      (show-queen (cdr rest-of-queens)))))

;(show-queen (queens 2))
;(show-queen (queens 3))
(show-queen (queens 4))

(show-queen (queens 8))
