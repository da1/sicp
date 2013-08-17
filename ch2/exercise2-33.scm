;; 問題2.33
; リスト操作の基本演算の，アキュムレーションとしての定義
(load "./ch2/2-2-3_Sequences_as_Conventional_Interfaces.scm")

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map square (list 1 2 3 4 5))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2) (list 3 4 5))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length (list 1 2 3 4 5))
