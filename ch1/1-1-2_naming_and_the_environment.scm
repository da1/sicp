;; 1.1.2 名前と環境
;; プログラム言語の重要な点は，名前を使って計算オブジェクトを指す手段を用意することである

(define size 2)
size
(* 5 size)

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))
circumference

;; 値と記号を対応付け，それをあとから取り出せるようにする．
;; この記憶を環境という．
