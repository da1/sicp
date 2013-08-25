;; 問題2.43
; Louis Reasoner は問題2.42をやるのにおそろしく時間がかかった
; 彼の手続きqueensは動くがすごく遅い
; Eva Lu Atorに助けを求めたとき，彼女は彼がflatmapの写像の入れ子の順番を入れ替えて

(flatmap
  (lambda (new-row)
    (map (lambda (rest-of-queens)
           (adjoin-position new-row k rest-of-queens))
         (queen-cols (- k 1))))
  (enumerate-interval 1 board-size))

; 元の版
(flatmap
  (lambda (rest-of-queens)
    (map (lambda (new-row)
            (adjoin-position new-row k rest-of-queens))
         (enumerate-interval 1 board-size)))
  (queen-cols (- k 1)))

; と書いていた
; なぜ遅いのか
; 問題2.42の処理時間をTとしたときLouisのプログラムはどのくらいかかるのか

; 参考
; http://www.serendip.ws/archives/798

; 遅い方だと，enumerate-interval分だけqueen-colsを計算する必要がある
; 通常の版だと，enumerate-intervalのループは一回ですむ

; T * board-size ^ borad-size
; の時間がかかる
