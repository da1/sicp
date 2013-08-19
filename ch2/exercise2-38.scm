;; 問題2.38
; accumulateは並びの先頭の要素を右方のすべての要素を組み合わせた結果に組み合わせるので，
; fold-rightとしても知られている．
; 逆向きに仕事をしながら要素を組み合わせる他は，fold-leftになる．

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
; 3/2
; (/ 1 (/ 2 (/ 3 1)))

(fold-left / 1 (list 1 2 3))
; 1/6
; (/ (/ (/ 1 1) 2) 3)

(fold-right list '() (list 1 2 3))
; (1 (2 (3 ())))

(fold-left list '() (list 1 2 3))
; (1 (2 (3 ())))

; fold-right と fold-leftが同じ値を生じるためにopが満たすべき性質はなにか
; 結合法則

; 交換法則
; http://eomole.hatenablog.com/entry/2013/06/17/181820
; 交換法則は無くてもいい場合がある。
; 関数プログラミング入門に説明がある（らしい）
