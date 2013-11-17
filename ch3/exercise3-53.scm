;; 問題3.53
;; プログラムを走らせずに，
(define s (cons-stream 1 (add-streams s s)))
;;で定義するストリームの要素を述べよ
;; 自分自身を足してる．2のべき乗ストリーム

(show-stream s 0 10)

